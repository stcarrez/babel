with GNAT.Command_Line;  use GNAT.Command_Line;
with GNAT.IO;  use GNAT.IO;
with GNAT.Traceback.Symbolic;
with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Text_IO;
with Babel;
with Babel.Files;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Util.Encoders;
with Util.Encoders.Base16;
with Util.Log.Loggers;
with Babel.Filters;
with Babel.Files.Buffers;
with Babel.Files.Queues;
with Babel.Stores.Local;
with Babel.Strategies.Default;
with Babel.Strategies.Workers;
with Babel.Base.Text;
with Babel.Base.Users;
with Babel.Streams;
with Babel.Streams.XZ;
with Babel.Streams.Cached;
with Babel.Streams.Files;
with Tar;
procedure babel_main is

   use Ada.Strings.Unbounded;

   Out_Dir     : Ada.Strings.Unbounded.Unbounded_String;
   Dir         : Babel.Files.Directory_Type;
   Hex_Encoder : Util.Encoders.Base16.Encoder;
   Exclude     : aliased Babel.Filters.Exclude_Directory_Filter_Type;
   Local       : aliased Babel.Stores.Local.Local_Store_Type;
   Backup      : aliased Babel.Strategies.Default.Default_Strategy_Type;
   Buffers     : aliased Babel.Files.Buffers.Buffer_Pool;
   Store       : aliased Babel.Stores.Local.Local_Store_Type;
   Database    : aliased Babel.Base.Text.Text_Database;
   Queue       : aliased Babel.Files.Queues.File_Queue;
   Debug       : Boolean := False;
   Task_Count  : Positive := 2;

   --
--     procedure Print_Sha (Path : in String;
--                          File : in out Babel.Files.File) is
--        Sha : constant String := Hex_Encoder.Transform (File.SHA1);
--     begin
--        Put_Line (Path & "/" & To_String (File.Name) & " => " & Sha);
--     end Print_Sha;

   procedure Usage is
   begin
      Ada.Text_IO.Put_Line ("babel [-d] [-t count] {command} [options]");
      Ada.Text_IO.Put_Line (" -d        Debug mode");
      Ada.Text_IO.Put_Line (" -t count  Number of tasks to create");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Commands:");
      Ada.Text_IO.Put_Line (" copy <dst-dir> <src-dir>");
      Ada.Text_IO.Put_Line (" scan <src-dir>");
      Ada.Command_Line.Set_Exit_Status (2);
   end Usage;

   procedure Configure (Strategy : in out Babel.Strategies.Default.Default_Strategy_Type) is
   begin
      Strategy.Set_Filters (Exclude'Unchecked_Access);
      Strategy.Set_Stores (Read => Local'Unchecked_Access, Write => Store'Unchecked_Access);
      Strategy.Set_Buffers (Buffers'Unchecked_Access);
      Strategy.Set_Database (Database'Unchecked_Access);
      Strategy.Set_Queue (Queue'Unchecked_Access);
   end Configure;

   package Backup_Workers is
      new Babel.Strategies.Workers (Babel.Strategies.Default.Default_Strategy_Type);

   procedure Do_Backup (Count : in Positive) is
      Workers   : Backup_Workers.Worker_Type (Count);
      Container : Babel.Files.Default_Container;
      Queue     : Babel.Files.Queues.Directory_Queue;
   begin
      Babel.Files.Queues.Add_Directory (Queue, Dir);
      Configure (Backup);
      Backup_Workers.Configure (Workers, Configure'Access);
      Backup_Workers.Start (Workers);
      Backup.Scan (Queue, Container);
      Backup_Workers.Finish (Workers, Database);
   end Do_Backup;

   procedure Do_Copy is
      Dst : constant String := GNAT.Command_Line.Get_Argument;
      Src : constant String := GNAT.Command_Line.Get_Argument;
   begin
      Dir := Babel.Files.Allocate (Name => Src,
                                   Dir  => Babel.Files.NO_DIRECTORY);
      --  Exclude.Add_Exclude (".svn");
      --  Exclude.Add_Exclude ("obj");
      Babel.Files.Buffers.Create_Pool (Into => Buffers, Count => 10, Size => 20_000_000);
      Store.Set_Root_Directory (Dst);
      Local.Set_Root_Directory ("");

      Do_Backup (Task_Count);
      Database.Save ("database.txt");
   end Do_Copy;

   procedure Do_Scan is
      Src : constant String := GNAT.Command_Line.Get_Argument;
   begin
      Dir := Babel.Files.Allocate (Name => Src,
                                   Dir  => Babel.Files.NO_DIRECTORY);
      Exclude.Add_Exclude (".svn");
      Exclude.Add_Exclude ("obj");
      Babel.Files.Buffers.Create_Pool (Into => Buffers, Count => 10, Size => 1_000_000);
      Local.Set_Root_Directory (Src);

      Do_Backup (Task_Count);
      Database.Save ("database.txt");
   end Do_Scan;

begin
   Util.Log.Loggers.Initialize ("babel.properties");
   Initialize_Option_Scan (Stop_At_First_Non_Switch => True, Section_Delimiters => "targs");

   --  Parse the command line
   loop
      case Getopt ("* v o: t:") is
         when ASCII.NUL =>
            exit;

         when 'o' =>
            Out_Dir := To_Unbounded_String (Parameter & "/");

         when 'd' =>
            Debug := True;

         when 't' =>
            Task_Count := Positive'Value (Parameter);

         when '*' =>
            exit;

         when others =>
            null;
      end case;
   end loop;
   if Ada.Command_Line.Argument_Count = 0 then
      Usage;
      return;
   end if;
   declare
      Cmd_Name  : constant String := Full_Switch;
   begin
      if Cmd_Name = "copy" then
         Do_Copy;
      elsif Cmd_Name = "scan" then
         Do_Scan;
      else
         Usage;
      end if;
   end;

exception
   when E : Invalid_Switch =>
      Ada.Text_IO.Put_Line ("Invalid option: " & Ada.Exceptions.Exception_Message (E));
      Usage;

   when E : others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
      Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      Ada.Command_Line.Set_Exit_Status (1);

end babel_main;

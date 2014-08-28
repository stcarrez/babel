with GNAT.IO;  use GNAT.IO;
with Babel;
with Babel.Files;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Util.Encoders;
with Util.Encoders.Base16;
with Babel.Filters;
with Babel.Files.Buffers;
with Babel.Stores.Local;
with Babel.Strategies.Default;
with Babel.Strategies.Workers;
procedure babel_main is

   use Ada.Strings.Unbounded;

   Dir         : Babel.Files.Directory;
   Hex_Encoder : Util.Encoders.Base16.Encoder;
   Exclude     : aliased Babel.Filters.Exclude_Directory_Filter_Type;
   Local       : aliased Babel.Stores.Local.Local_Store_Type;
   Backup      : aliased Babel.Strategies.Default.Default_Strategy_Type;
   Buffers     : aliased Babel.Files.Buffers.Buffer_Pools.Pool;

   procedure Print_Sha (Path : in String;
                        File : in out Babel.Files.File) is
      Sha : constant String := Hex_Encoder.Transform (File.SHA1);
   begin
      Put_Line (Path & "/" & To_String (File.Name) & " => " & Sha);
   end Print_Sha;

   procedure Do_Backup (Count : in Positive) is
      Workers : Babel.Strategies.Workers.Worker_Type (Count);
   begin
      Babel.Strategies.Workers.Start (Workers, Backup'Unchecked_Access);
      Backup.Scan (".");
   end Do_Backup;

begin
   Exclude.Add_Exclude (".svn");
   Exclude.Add_Exclude ("obj");
   Babel.Files.Buffers.Create_Pool (Buffers, 10, 1_000_000);
   Backup.Set_Filters (Exclude'Unchecked_Access);
   Backup.Set_Stores (Local'Unchecked_Access, Local'Unchecked_Access);
   Backup.Set_Buffers (Buffers'Unchecked_Access);

   Put_Line ("Size: " & Natural'Image (Ada.Directories.File_Size'Size));
   Do_Backup (2);
--     Bkp.Files.Scan (".", Dir);
--     Bkp.Files.Iterate_Files (".", Dir, 10, Print_Sha'Access);
   Put_Line ("Total size: " & Ada.Directories.File_Size'Image (Dir.Tot_Size));
   Put_Line ("File count: " & Natural'Image (Dir.Tot_Files));
   Put_Line ("Dir count:  " & Natural'Image (Dir.Tot_Dirs));
end babel_main;

-----------------------------------------------------------------------
--  babel-commands -- Commands for Babel
--  Copyright (C) 2011, 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Command_Line;

with GNAT.Command_Line;

with Util.Log.Loggers;
with Babel.Commands.Save;
package body Babel.Commands is

   use Ada.Strings.Unbounded;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Babel.Commands");

   Commands : Command_Maps.Map;


   --  ------------------------------
   --  Write the command usage.
   --  ------------------------------
   procedure Usage (Cmd : in Command) is
   begin
      null;
   end Usage;

   --  ------------------------------
   --  Print a message on the standard output.
   --  ------------------------------
   procedure Print (Cmd     : in Command;
                    Message : in String) is
      pragma Unreferenced (Cmd);
   begin
      Ada.Text_IO.Put_Line (Message);
   end Print;

   --  ------------------------------
   --  Print dynamo usage
   --  ------------------------------
   procedure Usage is
      use Ada.Text_IO;
   begin
--        Put_Line (Gen.Configs.RELEASE);
      New_Line;
      Put ("Usage: ");
      Put (Ada.Command_Line.Command_Name);
      Put_Line (" [-v] [-o directory] [-t templates] {command} {arguments}");
      Put_Line ("where:");
      Put_Line ("   -v           Print the version, configuration and installation paths");
      Put_Line ("   -o directory Directory where the Ada mapping files are generated");
      Put_Line ("   -t templates Directory where the Ada templates are defined");
      Put_Line ("   -c dir       Directory where the Ada templates "
                & "and configurations are defined");
   end Usage;

   --  ------------------------------
   --  Print dynamo short usage.
   --  ------------------------------
   procedure Short_Help_Usage is
      use Ada.Text_IO;
   begin
      New_Line;
      Put ("Type '");
      Put (Ada.Command_Line.Command_Name);
      Put_Line (" help' for the list of commands.");
   end Short_Help_Usage;

   --  ------------------------------
   --  Execute the command with the arguments.
   --  ------------------------------
   procedure Execute (Cmd       : in Help_Command) is
      pragma Unreferenced (Cmd);

      procedure Print (Position : in Command_Maps.Cursor);

      use Ada.Text_IO;
      use GNAT.Command_Line;

      procedure Print (Position : in Command_Maps.Cursor) is
         Name : constant Unbounded_String := Command_Maps.Key (Position);
      begin
         Put_Line ("   " & To_String (Name));
      end Print;

      Name : constant String := Get_Argument;

   begin
      Log.Debug ("Execute command {0}", Name);

      if Name'Length = 0 then
         Usage;
         New_Line;
         Put ("Type '");
         Put (Ada.Command_Line.Command_Name);
         Put_Line (" help {command}' for help on a specific command.");
         New_Line;
         Put_Line ("Available subcommands:");

         Commands.Iterate (Process => Print'Access);
      else
         declare
            Target_Cmd : constant Command_Access := Find_Command (Name);
         begin
            if Target_Cmd = null then
               Log.Error ("Unknown command {0}", Name);
            else
               Target_Cmd.Help;
            end if;
         end;
      end if;
   end Execute;

   --  ------------------------------
   --  Write the help associated with the command.
   --  ------------------------------
   procedure Help (Cmd       : in Help_Command) is
   begin
      null;
   end Help;

   --  ------------------------------
   --  Register the command under the given  name.
   --  ------------------------------
   procedure Add_Command (Cmd  : in Command_Access;
                          Name : in String) is
   begin
      Commands.Include (Key => To_Unbounded_String (Name), New_Item => Cmd);
   end Add_Command;

   --  ------------------------------
   --  Find the command having the given name.
   --  ------------------------------
   function Find_Command (Name : in String) return Command_Access is
      Pos : constant Command_Maps.Cursor := Commands.Find (To_Unbounded_String (Name));
   begin
      if Command_Maps.Has_Element (Pos) then
         return Command_Maps.Element (Pos);
      else
         return null;
      end if;
   end Find_Command;

   --  Save command.
   Save_Cmd       : aliased Babel.Commands.Save.Command;

   --  Help command.
   Help_Cmd           : aliased Help_Command;
begin
   Add_Command (Name => "help", Cmd => Help_Cmd'Access);
   Add_Command (Name => "save", Cmd => Save_Cmd'Access);
end Babel.Commands;

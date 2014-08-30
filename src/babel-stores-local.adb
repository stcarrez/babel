-----------------------------------------------------------------------
--  bkp-stores-local -- Store management for local files
--  Copyright (C) 2014 Stephane.Carrez
--  Written by Stephane.Carrez (Stephane.Carrez@gmail.com)
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
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO;
with Util.Log.Loggers;
with Util.Files;
with Interfaces;
package body Babel.Stores.Local is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Babel.Stores.Local");

   function Get_File_Size (Ent : in Ada.Directories.Directory_Entry_Type) return Ada.Directories.File_Size is
      Size : Ada.Directories.File_Size;
   begin
      Size := Ada.Directories.Size (Ent);
      return Size;

   exception
      when Constraint_Error =>
         return Ada.Directories.File_Size (Interfaces.Unsigned_32'Last);
   end Get_File_Size;

   --  ------------------------------
   --  Get the absolute path to access the local file.
   --  ------------------------------
   function Get_Absolute_Path (Store : in Local_Store_Type;
                               Path  : in String) return String is
   begin
      if Ada.Strings.Unbounded.Length (Store.Root_Dir) = 0 then
         return Path;
      else
         return Util.Files.Compose (Ada.Strings.Unbounded.To_String (Store.Root_Dir), Path);
      end if;
   end Get_Absolute_Path;

   procedure Read (Store : in out Local_Store_Type;
                   Path  : in String;
                   Into  : in out Babel.Files.Buffers.Buffer) is
      use Ada.Strings.Unbounded;
      use type Ada.Streams.Stream_Element_Offset;

      File     : Ada.Streams.Stream_IO.File_Type;
      Abs_Path : constant String := Store.Get_Absolute_Path (Path);
   begin
      Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Abs_Path);
      Ada.Streams.Stream_IO.Read (File, Into.Data, Into.Last);
      Ada.Streams.Stream_IO.Close (File);
      Log.Info ("Read {0} -> {1}", Abs_Path, Ada.Streams.Stream_Element_Offset'Image (Into.Last));
   end Read;


   procedure Write (Store : in out Local_Store_Type;
                    Path  : in String;
                    Into  : in Babel.Files.Buffers.Buffer) is
      use Ada.Strings.Unbounded;
      use type Ada.Streams.Stream_Element_Offset;

      File     : Ada.Streams.Stream_IO.File_Type;
      Abs_Path : constant String := Store.Get_Absolute_Path (Path);
   begin
      Log.Info ("Write {0}", Abs_Path);

      begin
         Ada.Streams.Stream_IO.Create (File, Ada.Streams.Stream_IO.Out_File, Abs_Path);
      exception
         when Ada.Streams.Stream_IO.Name_Error =>
            Log.Info ("Create {0}", Abs_Path);
            Ada.Streams.Stream_IO.Create (File, Ada.Streams.Stream_IO.Out_File, Abs_Path);
      end;
      Ada.Streams.Stream_IO.Write (File, Into.Data (Into.Data'First .. Into.Last));
      Ada.Streams.Stream_IO.Close (File);
   end Write;

   procedure Scan (Store  : in out Local_Store_Type;
                   Path   : in String;
                   Into   : in out Babel.Files.File_Container'Class;
                   Filter : in Babel.Filters.Filter_Type'Class) is
      use Ada.Directories;
      use type Babel.Files.Directory_Vector_Access;

      Search_Filter : constant Ada.Directories.Filter_Type := (Ordinary_File => True,
                                                               Ada.Directories.Directory => True,
                                                               Special_File => False);
      Search   : Search_Type;
      Ent      : Directory_Entry_Type;
      New_File : Babel.Files.File;
      Child    : Babel.Files.Directory;
   begin
      Log.Info ("Scan directory {0}", Path);

      Start_Search (Search, Directory => Path, Pattern => "*", Filter => Search_Filter);
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Ent);
         declare
            Name : constant String    := Simple_Name (Ent);
            Kind : constant File_Kind := Ada.Directories.Kind (Ent);
         begin
            if Kind = Ordinary_File then
               if Filter.Is_Accepted (Kind, Path, Name) then
                  New_File.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
                  New_File.Path := Ada.Strings.Unbounded.To_Unbounded_String (Path);
                  New_File.Size := Get_File_Size (Ent);
                  Into.Add_File (Path, New_File);
               end if;
            elsif Name /= "." and Name /= ".." and Filter.Is_Accepted (Kind, Path, Name) then
--                 if Into.Children = null then
--                    Into.Children := new Babel.Files.Directory_Vector;
--                 end if;
--                 Child.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
--                 Into.Children.Append (Child);
--                 Into.Tot_Dirs := Into.Tot_Dirs + 1;
               Into.Add_Directory (Path, Name);
            end if;
         end;
      end loop;

   exception
      when E : others =>
         Log.Error ("Exception ", E);
   end Scan;

   --  ------------------------------
   --  Set the root directory for the local store.
   --  ------------------------------
   procedure Set_Root_Directory (Store : in out Local_Store_Type;
                                 Path  : in String) is
   begin
      Store.Root_Dir := Ada.Strings.Unbounded.To_Unbounded_String (Path);
   end Set_Root_Directory;

end Babel.Stores.Local;

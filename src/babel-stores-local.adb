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
with Interfaces.C.Strings;
with System.OS_Constants;

with Util.Log.Loggers;
with Util.Files;
with Util.Systems.Os;
with Util.Streams.Raw;
with Interfaces;
package body Babel.Stores.Local is

   function Errno return Integer;
   pragma Import (C, errno, "__get_errno");

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Babel.Stores.Local");

   function Get_File_Size (Ent : in Ada.Directories.Directory_Entry_Type) return Babel.Files.File_Size is
      Size : Ada.Directories.File_Size;
   begin
      Size := Ada.Directories.Size (Ent);
      return Babel.Files.File_Size (Size);

   exception
      when Constraint_Error =>
         return Babel.Files.File_Size (Interfaces.Unsigned_32'Last);
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

   procedure Open (Store  : in out Local_Store_Type;
                   Stream : in out Util.Streams.Raw.Raw_Stream;
                   Path   : in String) is
      use Util.Systems.Os;
      use type Interfaces.C.int;

      File : Util.Systems.Os.File_Type;
      Name : Util.Systems.Os.Ptr := Interfaces.C.Strings.New_String (Path);
   begin
      File := Util.Systems.Os.Sys_Open (Name, O_CREAT + O_WRONLY, 8#644#);
      if File < 0 then
         if Errno = System.OS_Constants.ENOENT then
            declare
               Dir : constant String := Ada.Directories.Containing_Directory (Path);
            begin
               Ada.Directories.Create_Path (Dir);
            end;
            File := Util.Systems.Os.Sys_Open (Name, O_CREAT + O_WRONLY, 8#644#);
         end if;
      end if;
      if File < 0 then
         raise Ada.Streams.Stream_IO.Name_Error with "Cannot create " & Path;
      end if;
      Stream.Initialize (File);
      Interfaces.C.Strings.Free (Name);
   end Open;

   procedure Write (Store : in out Local_Store_Type;
                    Path  : in String;
                    Into  : in Babel.Files.Buffers.Buffer) is
      use Ada.Strings.Unbounded;
      use type Ada.Streams.Stream_Element_Offset;

      Abs_Path : constant String := Store.Get_Absolute_Path (Path);
      Stream   : Util.Streams.Raw.Raw_Stream;
   begin
      Log.Info ("Write {0}", Abs_Path);

      Store.Open (Stream, Abs_Path);
      Stream.Write (Into.Data (Into.Data'First .. Into.Last));
      Stream.Close;
   end Write;

   procedure Scan (Store  : in out Local_Store_Type;
                   Path   : in String;
                   Into   : in out Babel.Files.File_Container'Class;
                   Filter : in Babel.Filters.Filter_Type'Class) is
      use Ada.Directories;
      use type Babel.Files.File_Type;
      use type Babel.Files.Directory_Type;

      Search_Filter : constant Ada.Directories.Filter_Type := (Ordinary_File => True,
                                                               Ada.Directories.Directory => True,
                                                               Special_File => False);
      Search   : Search_Type;
      Ent      : Directory_Entry_Type;
   begin
      Log.Info ("Scan directory {0}", Path);

      Start_Search (Search, Directory => Path, Pattern => "*", Filter => Search_Filter);
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Ent);
         declare
            Name : constant String    := Simple_Name (Ent);
            Kind : constant File_Kind := Ada.Directories.Kind (Ent);
            File : Babel.Files.File_Type;
            Dir  : Babel.Files.Directory_Type;
         begin
            if Kind = Ordinary_File then
               if Filter.Is_Accepted (Kind, Path, Name) then
                  File := Into.Find (Name);
                  if File = Babel.Files.NO_FILE then
                     File := Into.Create (Name);
                  end if;
                  Babel.Files.Set_Size (File, Get_File_Size (Ent));
                  Into.Add_File (File);
               end if;
            elsif Name /= "." and Name /= ".." and Filter.Is_Accepted (Kind, Path, Name) then
               Dir := Into.Find (Name);
               if Dir = Babel.Files.NO_DIRECTORY then
                  Dir := Into.Create (Name);
               end if;
               Into.Add_Directory (Dir);
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

-----------------------------------------------------------------------
--  babel-files-maps -- Hash maps for files and directories
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

package body Babel.Files.Maps is

   --  ------------------------------
   --  Find the file with the given name in the file map.
   --  ------------------------------
   function Find (From : in File_Map;
                  Name : in String) return File_Cursor is
   begin
      return From.Find (Key => Name'Unrestricted_Access);
   end Find;

   --  ------------------------------
   --  Find the file with the given name in the file map.
   --  ------------------------------
   function Find (From : in File_Map;
                  Name : in String) return File_Type is
      Pos : constant File_Cursor := From.Find (Key => Name'Unrestricted_Access);
   begin
      if File_Maps.Has_Element (Pos) then
         return File_Maps.Element (Pos);
      else
         return NO_FILE;
      end if;
   end Find;

   --  ------------------------------
   --  Insert the file in the file map.
   --  ------------------------------
   procedure Insert (Into : in out File_Map;
                     File : in File_Type) is
   begin
      Into.Insert (Key => File.Name'Unrestricted_Access, New_Item => File);
   end Insert;

   --  ------------------------------
   --  Find the directory with the given name in the directory map.
   --  ------------------------------
   function Find (From : in Directory_Map;
                  Name : in String) return Directory_Cursor is
   begin
      return From.Find (Key => Name'Unrestricted_Access);
   end Find;

   --  ------------------------------
   --  Find the directory with the given name in the directory map.
   --  ------------------------------
   function Find (From : in Directory_Map;
                  Name : in String) return Directory_Type is
      Pos : constant Directory_Cursor := From.Find (Key => Name'Unrestricted_Access);
   begin
      if Directory_Maps.Has_Element (Pos) then
         return Directory_Maps.Element (Pos);
      else
         return NO_DIRECTORY;
      end if;
   end Find;

   --  ------------------------------
   --  Add the file with the given name in the container.
   --  ------------------------------
   overriding
   procedure Add_File (Into    : in out Differential_Container;
                       Element : in File_Type) is
      use type ADO.Identifier;
   begin
      if Element.Id = ADO.NO_IDENTIFIER then
         Into.Known_Files.Insert (Element.Name'Unrestricted_Access, Element);
      end if;
   end Add_File;

   --  ------------------------------
   --  Add the directory with the given name in the container.
   --  ------------------------------
   overriding
   procedure Add_Directory (Into    : in out Differential_Container;
                            Element : in Directory_Type) is
      use type ADO.Identifier;
   begin
      if Element.Id = ADO.NO_IDENTIFIER then
         Into.Known_Dirs.Insert (Element.Name'Unrestricted_Access, Element);
      end if;
   end Add_Directory;

   --  ------------------------------
   --  Create a new file instance with the given name in the container.
   --  ------------------------------
   overriding
   function Create (Into : in Differential_Container;
                    Name : in String) return File_Type is
   begin
      return Allocate (Name => Name,
                       Dir  => Into.Current);
   end Create;

   --  ------------------------------
   --  Create a new directory instance with the given name in the container.
   --  ------------------------------
   overriding
   function Create (Into : in Differential_Container;
                    Name : in String) return Directory_Type is
   begin
      return Allocate (Name => Name,
                       Dir  => Into.Current);
   end Create;

   --  ------------------------------
   --  Find the file with the given name in this file container.
   --  Returns NO_FILE if the file was not found.
   --  ------------------------------
   overriding
   function Find (From : in Differential_Container;
                  Name : in String) return File_Type is
   begin
      return Find (From.Known_Files, Name);
   end Find;

   --  ------------------------------
   --  Find the directory with the given name in this file container.
   --  Returns NO_DIRECTORY if the directory was not found.
   --  ------------------------------
   overriding
   function Find (From : in Differential_Container;
                  Name : in String) return Directory_Type is
   begin
      return Find (From.Known_Dirs, Name);
   end Find;

   --  ------------------------------
   --  Set the directory object associated with the container.
   --  ------------------------------
   overriding
   procedure Set_Directory (Into      : in out Differential_Container;
                            Directory : in Directory_Type) is
   begin
      Default_Container (Into).Set_Directory (Directory);
      Into.Known_Files.Clear;
      Into.Known_Dirs.Clear;
   end Set_Directory;

   --  ------------------------------
   --  Prepare the differential container by setting up the known files and known
   --  directories.  The <tt>Update</tt> procedure is called to give access to the
   --  maps that can be updated.
   --  ------------------------------
   procedure Prepare (Container : in out Differential_Container;
                      Update    : access procedure (Files : in out File_Map;
                                                    Dirs  : in out Directory_Map)) is
   begin
      Update (Container.Known_Files, Container.Known_Dirs);
   end Prepare;

end Babel.Files.Maps;

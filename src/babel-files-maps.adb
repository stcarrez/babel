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
   --  Add the file with the given name in the container.
   --  ------------------------------
   procedure Add_File (Into    : in out Differential_Container;
                       Element : in File_Type) is
      use type ADO.Identifier;
   begin
      if Element.Id = ADO.NO_IDENTIFIER then
         Into.Files.Insert (Element.Name'Unrestricted_Access, Element);
      end if;
   end Add_File;

   --  ------------------------------
   --  Add the directory with the given name in the container.
   --  ------------------------------
   procedure Add_Directory (Into    : in out Differential_Container;
                            Element : in Directory_Type) is
      use type ADO.Identifier;
   begin
      if Element.Id = ADO.NO_IDENTIFIER then
         Into.Files.Children (Element.Name'Unrestricted_Access, Element);
      end if;
   end Add_Directory;

   --  ------------------------------
   --  Create a new file instance with the given name in the container.
   --  ------------------------------
   function Create (Into : in Differential_Container;
                    Name : in String) return File_Type is
   begin
      return Allocate (Name => Name,
                       Dir  => Into.Current);
   end Create;

   --  ------------------------------
   --  Create a new directory instance with the given name in the container.
   --  ------------------------------
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
   function Find (From : in Differential_Container;
                  Name : in String) return File_Type is
   begin
      return Find (From.Files, Name);
   end Find;

   --  ------------------------------
   --  Find the directory with the given name in this file container.
   --  Returns NO_DIRECTORY if the directory was not found.
   --  ------------------------------
   function Find (From : in Differential_Container;
                  Name : in String) return Directory_Type is
   begin
      return Find (From.Children, Name);
   end Find;

end Babel.Files.Maps;

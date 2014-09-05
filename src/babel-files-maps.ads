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

with Ada.Containers.Hashed_Maps;
with Util.Strings;

package Babel.Files.Maps is

   --  Babel.Base.Get_File_Map (Directory, File_Map);
   --  Babel.Base.Get_Directory_Map (Directory, Dir_Map);
   --  File_Map.Find (New_File);
   --  Dir_Map.Find (New_File);
   --  Hash string -> File
   package File_Maps is new
     Ada.Containers.Hashed_Maps (Key_Type        => Util.Strings.Name_Access,
                                 Element_Type    => File_Type,
                                 Hash            => Util.Strings.Hash,
                                 Equivalent_Keys => Util.Strings."=",
                                 "="             => "=");

   subtype File_Map is File_Maps.Map;
   subtype File_Cursor is File_Maps.Cursor;

   --  Find the file with the given name in the file map.
   function Find (From : in File_Map;
                  Name : in String) return File_Cursor;

   --  Hash string -> Directory
   package Directory_Maps is new
     Ada.Containers.Hashed_Maps (Key_Type        => Util.Strings.Name_Access,
                                 Element_Type    => Directory_Type,
                                 Hash            => Util.Strings.Hash,
                                 Equivalent_Keys => Util.Strings."=",
                                 "="             => "=");

   subtype Directory_Map is Directory_Maps.Map;
   subtype Directory_Cursor is Directory_Maps.Cursor;


   type Differential_Container is new Babel.Files.File_Container with private;

   --  Add the file with the given name in the container.
   procedure Add_File (Into    : in out Differential_Container;
                       Element : in File_Type);

   --  Add the directory with the given name in the container.
   procedure Add_Directory (Into    : in out Differential_Container;
                            Element : in Directory_Type);

   --  Create a new file instance with the given name in the container.
   function Create (Into : in Differential_Container;
                    Name : in String) return File_Type;

   --  Create a new directory instance with the given name in the container.
   function Create (Into : in Differential_Container;
                    Name : in String) return Directory_Type;

   --  Find the file with the given name in this file container.
   --  Returns NO_FILE if the file was not found.
   function Find (From : in Differential_Container;
                  Name : in String) return File_Type;

   --  Find the directory with the given name in this file container.
   --  Returns NO_DIRECTORY if the directory was not found.
   function Find (From : in Differential_Container;
                  Name : in String) return Directory_Type;

private

   type Differential_Container is new Babel.Files.File_Container with record
      Current  : Directory_Type;
      Files    : File_Map;
      Children : Directory_Map;
   end record;

end Babel.Files.Maps;

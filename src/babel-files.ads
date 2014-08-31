-----------------------------------------------------------------------
--  bkp-files -- File and directories
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
with Ada.Strings.Unbounded;
with Ada.Calendar;
with Ada.Directories;
with Ada.Containers.Vectors;
with Util.Encoders.SHA1;
with Util.Concurrent.Fifos;
with Util.Strings.Vectors;
with ADO;
with Babel.Base.Models;
package Babel.Files is

   NO_IDENTIFIER : constant ADO.Identifier := ADO.NO_IDENTIFIER;

   subtype File_Identifier is ADO.Identifier;
   subtype Directory_Identifier is ADO.Identifier;
   subtype File_Size is Long_Long_Integer;
   type File_Mode is mod 2**16;
   type Uid_Type is mod 2**16;
   type Gid_Type is mod 2**16;

   type File_Type is private;
   type File_Type_Array is array (Positive range <>) of File_Type;
   type File_Type_Array_Access is access all File_Type_Array;

   type Directory_Type is private;
   type Directory_Type_Array is array (Positive range <>) of Directory_Type;
   type Directory_Type_Array_Access is access all Directory_Type_Array;

   NO_DIRECTORY : constant Directory_Type;

   type Status_Type is mod 2**16;

   --  The file was modified.
   FILE_MODIFIED : constant Status_Type := 16#0001#;

   --  There was some error while processing this file.
   FILE_ERROR    : constant Status_Type := 16#8000#;

   --  The SHA1 signature for the file is known and valid.
   FILE_HAS_SHA1 : constant Status_Type := 16#0002#;

   --  Allocate a File_Type entry with the given name for the directory.
   function Allocate (Name : in String;
                      Dir  : in Directory_Type) return File_Type;

   type File (Len : Positive) is record
      Id     : File_Identifier := NO_IDENTIFIER;
      Size   : File_Size       := 0;
      Dir    : Directory_Type  := NO_DIRECTORY;
      Mode   : File_Mode       := 8#644#;
      User   : Uid_Type        := 0;
      Group  : Gid_Type        := 0;
      Status : Status_Type     := 0;
      Date   : Ada.Calendar.Time;
      SHA1   : Util.Encoders.SHA1.Hash_Array;
      Name   : aliased String (1 .. Len);
   end record;

   --  Return true if the file was modified and need a backup.
   function Is_Modified (Element : in File_Type) return Boolean;

   --  Set the file as modified.
   procedure Set_Modified (Element : in File_Type);

   --  Return the path for the file.
   function Get_Path (Element : in File_Type) return String;

   type File_Container is limited interface;

   --  Add the file with the given name in the container.
   procedure Add_File (Into    : in out File_Container;
                       Path    : in String;
                       Element : in File) is abstract;

   --  Add the directory with the given name in the container.
   procedure Add_Directory (Into : in out File_Container;
                            Path : in String;
                            Name : in String) is abstract;


   procedure Compute_Sha1 (Path : in String;
                           Into : in out File);

--     package File_Vectors is
--        new Ada.Containers.Vectors (Index_Type   => Positive,
--                                    Element_Type => File,
--                                    "="          => "=");

--     type Directory_Vector;
--     type Directory_Vector_Access is access all Directory_Vector;

--     type Directory is record -- new File_Container with record
--        Name      : Ada.Strings.Unbounded.Unbounded_String;
--        Files     : File_Vectors.Vector;
--        Children  : Directory_Vector_Access;
--        Tot_Size  : Ada.Directories.File_Size;
--        Tot_Files : Natural := 0;
--        Tot_Dirs  : Natural := 0;
--        Depth     : Natural := 0;
--     end record;
--
--     package Directory_Vectors is
--        new Ada.Containers.Vectors (Index_Type   => Positive,
--                                    Element_Type => Directory,
--                                    "="          => "=");
--
--     type Directory_Vector is new Directory_Vectors.Vector with null record;
--
--     procedure Scan (Path : in String;
--                     Into : in out Directory_Type);
--
--     procedure Iterate_Files (Path   : in String;
--                              Dir    : in out Directory;
--                              Depth  : in Natural;
--                              Update : access procedure (P : in String; F : in out File));

private

   type Directory (Len : Positive) is record
      Id       : Directory_Identifier := NO_IDENTIFIER;
      Parent   : Directory_Type;
      Mode     : File_Mode := 8#755#;
      User     : Uid_Type  := 0;
      Group    : Gid_Type  := 0;
      Files    : File_Type_Array_Access;
      Children : Directory_Type_Array_Access;
      Name     : aliased String (1 .. Len);
   end record;

   type File_Type is access all File;

   type Directory_Type is access all Directory;

   NO_DIRECTORY : constant Directory_Type := null;

end Babel.Files;

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
with Ada.Calendar;
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

with Util.Encoders.SHA1;
with ADO;
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
   NO_FILE      : constant File_Type;

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

   --  Allocate a Directory_Type entry with the given name for the directory.
   function Allocate (Name : in String;
                      Dir  : in Directory_Type) return Directory_Type;

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

   --  Compare two files on their name and directory.
   function "<" (Left, Right : in File_Type) return Boolean;

   --  Return true if the file was modified and need a backup.
   function Is_Modified (Element : in File_Type) return Boolean;

   --  Return true if the file is a new file.
   function Is_New (Element : in File_Type) return Boolean;

   --  Set the file as modified.
   procedure Set_Modified (Element : in File_Type);

   --  Set the SHA1 signature that was computed for this file.
   --  If the computed signature is different from the current signature,
   --  the FILE_MODIFIED flag is set on the file.  The FILE_HAS_SHA1 flag
   --  is set on the file.
   procedure Set_Signature (Element   : in File_Type;
                            Signature : in Util.Encoders.SHA1.Hash_Array);

   --  Set the file size.  If the new size is different, the FILE_MODIFIED
   --  flag is set on the file.
   procedure Set_Size (Element : in File_Type;
                       Size    : in File_Size);

   --  Return the path for the file.
   function Get_Path (Element : in File_Type) return String;

   --  Return the path for the directory.
   function Get_Path (Element : in Directory_Type) return String;

   --  Return the SHA1 signature computed for the file.
   function Get_SHA1 (Element : in File_Type) return String;

   --  Return the file size.
   function Get_Size (Element : in File_Type) return File_Size;

   type File_Container is limited interface;

   --  Add the file with the given name in the container.
   procedure Add_File (Into    : in out File_Container;
                       Element : in File_Type) is abstract;

   --  Add the directory with the given name in the container.
   procedure Add_Directory (Into    : in out File_Container;
                            Element : in Directory_Type) is abstract;

   --  Create a new file instance with the given name in the container.
   function Create (Into : in File_Container;
                    Name : in String) return File_Type is abstract;

   --  Create a new directory instance with the given name in the container.
   function Create (Into : in File_Container;
                    Name : in String) return Directory_Type is abstract;

   --  Find the file with the given name in this file container.
   --  Returns NO_FILE if the file was not found.
   function Find (From : in File_Container;
                  Name : in String) return File_Type is abstract;

   --  Find the directory with the given name in this file container.
   --  Returns NO_DIRECTORY if the directory was not found.
   function Find (From : in File_Container;
                  Name : in String) return Directory_Type is abstract;

   --  Set the directory object associated with the container.
   procedure Set_Directory (Into      : in out File_Container;
                            Directory : in Directory_Type) is abstract;

   --  Execute the Process procedure on each directory found in the container.
   procedure Each_Directory (Container : in File_Container;
                             Process   : not null access
                               procedure (Dir : in Directory_Type)) is abstract;

   --  Execute the Process procedure on each file found in the container.
   procedure Each_File (Container : in File_Container;
                        Process   : not null access
                          procedure (F : in File_Type)) is abstract;

   type Default_Container is new File_Container with private;

   --  Add the file with the given name in the container.
   overriding
   procedure Add_File (Into    : in out Default_Container;
                       Element : in File_Type);

   --  Add the directory with the given name in the container.
   overriding
   procedure Add_Directory (Into    : in out Default_Container;
                            Element : in Directory_Type);

   --  Create a new file instance with the given name in the container.
   overriding
   function Create (Into : in Default_Container;
                    Name : in String) return File_Type;

   --  Create a new directory instance with the given name in the container.
   overriding
   function Create (Into : in Default_Container;
                    Name : in String) return Directory_Type;

   --  Find the file with the given name in this file container.
   --  Returns NO_FILE if the file was not found.
   overriding
   function Find (From : in Default_Container;
                  Name : in String) return File_Type;

   --  Find the directory with the given name in this file container.
   --  Returns NO_DIRECTORY if the directory was not found.
   overriding
   function Find (From : in Default_Container;
                  Name : in String) return Directory_Type;

   --  Set the directory object associated with the container.
   overriding
   procedure Set_Directory (Into      : in out Default_Container;
                            Directory : in Directory_Type);

   --  Execute the Process procedure on each directory found in the container.
   overriding
   procedure Each_Directory (Container : in Default_Container;
                             Process   : not null access
                               procedure (Dir : in Directory_Type));

   --  Execute the Process procedure on each file found in the container.
   overriding
   procedure Each_File (Container : in Default_Container;
                        Process   : not null access
                          procedure (F : in File_Type));

private

   type File_Type is access all File;

   type Directory (Len : Positive) is record
      Id       : Directory_Identifier := NO_IDENTIFIER;
      Parent   : Directory_Type;
      Mode     : File_Mode := 8#755#;
      User     : Uid_Type  := 0;
      Group    : Gid_Type  := 0;
      Files    : File_Type_Array_Access;
      Children : Directory_Type_Array_Access;
      Path     : Ada.Strings.Unbounded.Unbounded_String;
      Name     : aliased String (1 .. Len);
   end record;

   type Directory_Type is access all Directory;

   package File_Vectors is new
     Ada.Containers.Vectors (Index_Type   => Positive,
                             Element_Type => File_Type,
                             "="          => "=");

   package Directory_Vectors is new
     Ada.Containers.Vectors (Index_Type   => Positive,
                             Element_Type => Directory_Type,
                             "="          => "=");

   subtype Directory_Vector is Directory_Vectors.Vector;

   type Default_Container is new Babel.Files.File_Container with record
      Current : Directory_Type;
      Files   : File_Vectors.Vector;
      Dirs    : Directory_Vectors.Vector;
   end record;

   NO_DIRECTORY : constant Directory_Type := null;
   NO_FILE      : constant File_Type := null;

end Babel.Files;

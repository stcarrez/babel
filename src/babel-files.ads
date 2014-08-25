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
with Ada.Directories;
with Ada.Containers.Vectors;
with Util.Encoders.SHA1;
with Util.Concurrent.Fifos;
with Util.Strings.Vectors;
with Babel.Base.Models;
package Babel.Files is

   type File_Status is (NEW_FILE, FILE_MODIFIED, FILE_UNCHANGED);

   type File is record
      Name   : Ada.Strings.Unbounded.Unbounded_String;
      Size   : Ada.Directories.File_Size;
      SHA1   : Util.Encoders.SHA1.Hash_Array;
      Status : File_Status := FILE_UNCHANGED;
   end record;

   --  Return true if the file was modified and need a backup.
   function Is_Modified (Element : in File) return Boolean;

   type File_Container is limited interface;

   --  Add the file with the given name in the container.
   procedure Add_File (Into    : in out File_Container;
                       Path    : in String;
                       Element : in File) is abstract;

   --  Add the directory with the given name in the container.
   procedure Add_Directory (Into : in out File_Container;
                            Path : in String;
                            Name : in String) is abstract;

   --  package Babel.Strategies.Flow/Optimize/Small/Larges/Default/Immediate/Simple/Pipeline/Serial
   --  File name
   --  Directory info
   --  File ID or null
   --  File size         => sha1, file status, backup
   package File_Fifo is new Util.Concurrent.Fifos (Element_Type     => File,
                                                   Default_Size     => 100,
                                                   Clear_On_Dequeue => True);
--
--     procedure Execute (Backup : in out Small_Files_Strategy;
--                        Queue  : in out File_Queue) is
--     begin
--        loop
--           Queue.Dequeue (File, 1.0);
--           --  load file
--           --  compute sha1
--           --  if modified then backup file
--        end loop;
--
--     exception
--        when File_Fifo.Timeout =>
--           null;
--     end Execute;

   type File_Queue is limited new File_Container with record
      Queue       : File_Fifo.Fifo;
      Directories : Util.Strings.Vectors.Vector;
   end record;

   overriding
   procedure Add_File (Into    : in out File_Queue;
                       Path    : in String;
                       Element : in File);

   overriding
   procedure Add_Directory (Into : in out File_Queue;
                            Path : in String;
                            Name : in String);

   procedure Compute_Sha1 (Path : in String;
                           Into : in out File);

   package File_Vectors is
      new Ada.Containers.Vectors (Index_Type   => Positive,
                                  Element_Type => File,
                                  "="          => "=");

   type Directory_Vector;
   type Directory_Vector_Access is access all Directory_Vector;

   type Directory is record -- new File_Container with record
      Name      : Ada.Strings.Unbounded.Unbounded_String;
      Files     : File_Vectors.Vector;
      Children  : Directory_Vector_Access;
      Tot_Size  : Ada.Directories.File_Size;
      Tot_Files : Natural := 0;
      Tot_Dirs  : Natural := 0;
      Depth     : Natural := 0;
   end record;

   package Directory_Vectors is
      new Ada.Containers.Vectors (Index_Type   => Positive,
                                  Element_Type => Directory,
                                  "="          => "=");

   type Directory_Vector is new Directory_Vectors.Vector with null record;

   procedure Scan (Path : in String;
                   Into : in out Directory);

   procedure Iterate_Files (Path   : in String;
                            Dir    : in out Directory;
                            Depth  : in Natural;
                            Update : access procedure (P : in String; F : in out File));

end Babel.Files;

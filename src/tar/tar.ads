-----------------------------------------------------------------------
--  tar -- TAR file
--  Copyright (C) 2015 Stephane.Carrez
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
with Ada.Finalization;
with Ada.Streams;
with Ada.Strings.Unbounded;

with Util.Systems.Types;
with Util.Streams;

package Tar is

   --  The <tt>File_Info_Type</tt> describes a file entry stored in the TAR file.
   type File_Info_Type is record
      Name     : Ada.Strings.Unbounded.Unbounded_String;
      Mode     : Util.Systems.Types.mode_t;
      Uid      : Util.Systems.Types.uid_t;
      Gid      : Util.Systems.Types.gid_t;
      Size     : Util.Systems.Types.off_t;
      Mtime    : Util.Systems.Types.Time_Type;
      Chksum   : Natural;
      Typeflag : Natural;
      Linkname : Ada.Strings.Unbounded.Unbounded_String;
      Uname    : Ada.Strings.Unbounded.Unbounded_String;
      Gname    : Ada.Strings.Unbounded.Unbounded_String;
      Devmajor : Util.Systems.Types.dev_t;
      Devminor : Util.Systems.Types.dev_t;
      Prefix   : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type File_Type is limited private;

   --  Open the tar file for reading and use the given input stream to read the tar blocks.
   procedure Open (File  : in out File_Type;
                   Input : in Util.Streams.Input_Stream_Access);

   --  Returns true if the tar file contains another file entry.
   function Has_Element (File : in File_Type) return Boolean;

   --  Read the next file entry in the tar file.
   procedure Next (File : in out File_Type);

   --  Get the current file entry information.
   function Element (File : in File_Type) return File_Info_Type;

   --  Read the data for the current file entry.
   procedure Read (File   : in out File_Type;
                   Into   : out Ada.Streams.Stream_Element_Array;
                   Last   : out Ada.Streams.Stream_Element_Offset);

private

   type File_Type is limited new Ada.Finalization.Limited_Controlled with record
      Input       : Util.Streams.Input_Stream_Access;
      Output      : Util.Streams.Output_Stream_Access;
      Current     : File_Info_Type;
      Has_Current : Boolean := False;
      Remain      : Ada.Streams.Stream_Element_Count;
      Padding     : Ada.Streams.Stream_Element_Count;
   end record;

end Tar;

-----------------------------------------------------------------------
--  babel-streams-files -- Local file stream management
--  Copyright (C) 2014, 2015, 2016 Stephane.Carrez
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
with Interfaces.C.Strings;
with System.OS_Constants;
with Ada.Streams;
with Ada.Directories;
with Ada.IO_Exceptions;
with Interfaces.C;

with Util.Systems.Os;
with Util.Systems.Constants;
package body Babel.Streams.Files is

   use type Interfaces.C.int;

   function Sys_Fadvise (Fd     : in Util.Systems.Os.File_Type;
                         Offset : in Util.Systems.Types.off_t;
                         Length : in Util.Systems.Types.off_t;
                         Advice : in Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Sys_Fadvise, "posix_fadvise");

   --  ------------------------------
   --  Open the local file for reading and use the given buffer for the Read operation.
   --  ------------------------------
   procedure Open (Stream : in out Stream_Type;
                   Path   : in String;
                   Buffer : in Babel.Files.Buffers.Buffer_Access) is
      use type Util.Systems.Os.File_Type;

      Name : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String (Path);
      Fd   : Util.Systems.Os.File_Type;
      Res  : Interfaces.C.int;
   begin
      Fd := Util.Systems.Os.Sys_Open (Path  => Name,
                                      Flags => Util.Systems.Constants.O_RDONLY,
                                      Mode  => 0);
      Interfaces.C.Strings.Free (Name);
      Stream.Buffer := Buffer;
      Stream.File.Initialize (File => Fd);
      if Fd >= 0 then
         Res := Sys_Fadvise (Fd, 0, 0, 2);
      end if;
   end Open;

   --  ------------------------------
   --  Create a file and prepare for the Write operation.
   --  ------------------------------
   procedure Create (Stream : in out Stream_Type;
                     Path   : in String;
                     Mode   : in Util.Systems.Types.mode_t) is
      use type Util.Systems.Os.File_Type;
      use Util.Systems.Os;
      use type Interfaces.C.int;

      Name : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String (Path);
      File : Util.Systems.Os.File_Type;
   begin
      File := Util.Systems.Os.Sys_Open (Path  => Name,
                                        Flags => Util.Systems.Constants.O_WRONLY
                                        + Util.Systems.Constants.O_CREAT
                                        + Util.Systems.Constants.O_TRUNC,
                                        Mode  => Mode);
      if File < 0 then
         if Errno = System.OS_Constants.ENOENT then
            declare
               Dir : constant String := Ada.Directories.Containing_Directory (Path);
            begin
               Ada.Directories.Create_Path (Dir);
            end;
            File := Util.Systems.Os.Sys_Open (Name, O_CREAT + O_WRONLY, Mode);
         end if;
      end if;
      Interfaces.C.Strings.Free (Name);
      Stream.Buffer := null;
      Stream.File.Initialize (File => File);
      if File < 0 then
         raise Ada.IO_Exceptions.Name_Error with "Cannot create '" & Path & "'";
      end if;
    end Create;

   --  ------------------------------
   --  Read the data stream as much as possible and return the result in a buffer.
   --  The buffer is owned by the stream and need not be released.  The same buffer may
   --  or may not be returned by the next <tt>Read</tt> operation.
   --  A null buffer is returned when the end of the data stream is reached.
   --  ------------------------------
   overriding
   procedure Read (Stream : in out Stream_Type;
                   Buffer : out Babel.Files.Buffers.Buffer_Access) is
      use type Ada.Streams.Stream_Element_Offset;
   begin
      if Stream.Eof then
         Buffer := null;
      else
         Stream.File.Read (Stream.Buffer.Data, Stream.Buffer.Last);
         if Stream.Buffer.Last < Stream.Buffer.Data'First then
            Buffer := null;
         else
            Buffer := Stream.Buffer;
         end if;
         Stream.Eof := Stream.Buffer.Last < Stream.Buffer.Data'Last;
      end if;
   end Read;

   --  ------------------------------
   --  Write the buffer in the data stream.
   --  ------------------------------
   overriding
   procedure Write (Stream : in out Stream_Type;
                    Buffer : in Babel.Files.Buffers.Buffer_Access) is
   begin
      Stream.File.Write (Buffer.Data (Buffer.Data'First .. Buffer.Last));
   end Write;

   --  ------------------------------
   --  Close the data stream.
   --  ------------------------------
   overriding
   procedure Close (Stream : in out Stream_Type) is
   begin
      Stream.File.Close;
   end Close;

   --  ------------------------------
   --  Prepare to read again the data stream from the beginning.
   --  ------------------------------
   overriding
   procedure Rewind (Stream : in out Stream_Type) is
   begin
      Stream.File.Seek (0, Util.Systems.Types.SEEK_SET);
      Stream.Eof := False;
   end Rewind;

end Babel.Streams.Files;

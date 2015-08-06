-----------------------------------------------------------------------
--  babel-streams-files -- Local file stream management
--  Copyright (C) 2014, 2015 Stephane.Carrez
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

private with Util.Streams.Raw;

--  == Local Files Stream ==
--  The <tt>Babel.Streams.Files</tt> package provides the streams support to read and write
--  local files.
package Babel.Streams.Files is

   type Stream_Type is new Babel.Streams.Stream_Type with private;
   type Stream_Access is access all Stream_Type'Class;

   --  Open the local file for reading and use the given buffer for the Read operation.
   procedure Open (Stream : in out Stream_Type;
                   Path   : in String;
                   Buffer : in Babel.Files.Buffers.Buffer_Access);

   --  Create a file and prepare for the Write operation.
   procedure Create (Stream : in out Stream_Type;
                     Path   : in String;
                     Mode   : in Util.Systems.Types.mode_t);

   --  Read the data stream as much as possible and return the result in a buffer.
   --  The buffer is owned by the stream and need not be released.  The same buffer may
   --  or may not be returned by the next <tt>Read</tt> operation.
   --  A null buffer is returned when the end of the data stream is reached.
   overriding
   procedure Read (Stream : in out Stream_Type;
                   Buffer : out Babel.Files.Buffers.Buffer_Access);

   --  Write the buffer in the data stream.
   overriding
   procedure Write (Stream : in out Stream_Type;
                    Buffer : in Babel.Files.Buffers.Buffer_Access);

   --  Close the data stream.
   overriding
   procedure Close (Stream : in out Stream_Type);

   --  Prepare to read again the data stream from the beginning.
   overriding
   procedure Rewind (Stream : in out Stream_Type);

   --  Set the internal buffer that the stream can use.
   overriding
   procedure Set_Buffer (Stream : in out Stream_Type;
                         Buffer : in Babel.Files.Buffers.Buffer_Access);

private

   type Stream_Type is new Babel.Streams.Stream_Type with record
      File    : Util.Streams.Raw.Raw_Stream;
      Buffer  : Babel.Files.Buffers.Buffer_Access;
      Eof     : Boolean := False;
   end record;

end Babel.Streams.Files;

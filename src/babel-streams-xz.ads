-----------------------------------------------------------------------
--  babel-Streams-xz -- XZ/LZMA stream management
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

private with Lzma.Base;

--  == Compression and Decompression Stream ==
--  The <tt>Babel.Streams.XZ</tt> package provides a compression decompression stream on top
--  of the <tt>LZMA</tt> library.
package Babel.Streams.XZ is

   type Stream_Type is new Babel.Streams.Stream_Type with private;
   type Stream_Access is access all Stream_Type'Class;

   --  Set the input stream to decompress.
   procedure Set_Input (Stream : in out Stream_Type;
                        Input  : in Babel.Streams.Stream_Access);

   --  Set the output stream to write the compressed stream.
   procedure Set_Output (Stream : in out Stream_Type;
                         Output : in Babel.Streams.Stream_Access);

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

   --  Flush the data stream.
   overriding
   procedure Flush (Stream : in out Stream_Type);

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
      Input   : Babel.Streams.Stream_Access;
      Output  : Babel.Streams.Stream_Access;
      Context : aliased Lzma.Base.lzma_stream := Lzma.Base.LZMA_STREAM_INIT;
      Action  : Lzma.Base.lzma_action := Lzma.Base.LZMA_RUN;
      Buffer  : Babel.Files.Buffers.Buffer_Access;
   end record;

end Babel.Streams.XZ;

-----------------------------------------------------------------------
--  babel-streams-cached -- Cached stream management
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

--  == Cached Stream ==
--  The <tt>Babel.Streams.Cached</tt> package provides a cached stream where the whole
--  stream is read in one or several stream buffers.
package Babel.Streams.Cached is

   type Stream_Type is new Babel.Streams.Stream_Type with private;
   type Stream_Access is access all Stream_Type'Class;

   --  Load the file stream into the cache and use the buffer pool to obtain more buffers
   --  for the cache.
   procedure Load (Stream : in out Stream_Type;
                   File   : in out Babel.Streams.Stream_Type'Class;
                   Pool   : in out Babel.Files.Buffers.Buffer_Pool);

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

   --  Prepare to read again the data stream from the beginning.
   overriding
   procedure Rewind (Stream : in out Stream_Type);

private

   type Stream_Type is new Babel.Streams.Stream_Type with record
      Input   : Babel.Streams.Stream_Access;
      Output  : Babel.Streams.Stream_Access;
      Buffers : Babel.Files.Buffers.Buffer_Access_Vector;
      Current : Babel.Files.Buffers.Buffer_Access_Cursor;
   end record;

   --  Release the buffers associated with the cache.
   overriding
   procedure Finalize (Stream : in out Stream_Type);

end Babel.Streams.Cached;

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

package body Babel.Streams.Cached is

   package BAV renames Babel.Files.Buffers.Buffer_Access_Vectors;

   --  ------------------------------
   --  Load the file stream into the cache and use the buffer pool to obtain more buffers
   --  for the cache.
   --  ------------------------------
   procedure Load (Stream : in out Stream_Type;
                   File   : in out Babel.Streams.Stream_Type'Class;
                   Pool   : in out Babel.Files.Buffers.Buffer_Pool) is
      use type Babel.Files.Buffers.Buffer_Access;

      Buffer : Babel.Files.Buffers.Buffer_Access;
   begin
      loop
         File.Read (Buffer);
         exit when Buffer = null;
         Stream.Buffers.Append (Buffer);
         Pool.Get_Buffer (Buffer);
         File.Set_Buffer (Buffer);
      end loop;
      Stream.Current := Stream.Buffers.First;
   end Load;

   --  ------------------------------
   --  Read the data stream as much as possible and return the result in a buffer.
   --  The buffer is owned by the stream and need not be released.  The same buffer may
   --  or may not be returned by the next <tt>Read</tt> operation.
   --  A null buffer is returned when the end of the data stream is reached.
   --  ------------------------------
   overriding
   procedure Read (Stream : in out Stream_Type;
                   Buffer : out Babel.Files.Buffers.Buffer_Access) is
   begin
      if BAV.Has_Element (Stream.Current) then
         Buffer := BAV.Element (Stream.Current);
         BAV.Next (Stream.Current);
      else
         Buffer := null;
      end if;
   end Read;

   --  ------------------------------
   --  Write the buffer in the data stream.
   --  ------------------------------
   overriding
   procedure Write (Stream : in out Stream_Type;
                    Buffer : in Babel.Files.Buffers.Buffer_Access) is
   begin
      null;
   end Write;

   --  ------------------------------
   --  Prepare to read again the data stream from the beginning.
   --  ------------------------------
   overriding
   procedure Rewind (Stream : in out Stream_Type) is
   begin
      Stream.Current := Stream.Buffers.First;
   end Rewind;

   --  ------------------------------
   --  Release the buffers associated with the cache.
   --  ------------------------------
   overriding
   procedure Finalize (Stream : in out Stream_Type) is
      Buffer : Babel.Files.Buffers.Buffer_Access;
   begin
      while not Stream.Buffers.Is_Empty loop
         Buffer := Stream.Buffers.Last_Element;
         Babel.Files.Buffers.Release (Buffer);
         Stream.Buffers.Delete_Last;
      end loop;
      Babel.Streams.Stream_Type (Stream).Finalize;
   end Finalize;

end Babel.Streams.Cached;

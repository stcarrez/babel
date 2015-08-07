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
with Ada.Streams;

with Interfaces.C;

with Lzma.Container;
with Lzma.Check;
package body Babel.Streams.XZ is

   use type Interfaces.C.size_t;
   use type Ada.Streams.Stream_Element_Offset;
   use type Lzma.Base.lzma_action;
   use type Lzma.Base.lzma_ret;

   --  ------------------------------
   --  Set the input stream to decompress.
   --  ------------------------------
   procedure Set_Input (Stream : in out Stream_Type;
                        Input  : in Babel.Streams.Stream_Access) is
      Result : Lzma.Base.lzma_ret;
   begin
      Stream.Input := Input;
      Stream.Action := Lzma.Base.LZMA_RUN;
      Result := Lzma.Container.lzma_stream_decoder (Stream.Context'Unchecked_Access,
                                                    Long_Long_Integer'Last,
                                                    Lzma.Container.LZMA_CONCATENATED);
   end Set_Input;

   --  ------------------------------
   --  Set the output stream to write the compressed stream.
   --  ------------------------------
   procedure Set_Output (Stream : in out Stream_Type;
                         Output : in Babel.Streams.Stream_Access) is
      Result : Lzma.Base.lzma_ret;
   begin
      Stream.Output := Output;
      Stream.Action := Lzma.Base.LZMA_RUN;
      Result := Lzma.Container.lzma_easy_encoder (Stream.Context'Unchecked_Access, 6,
                                                  Lzma.Check.LZMA_CHECK_CRC64);
      Stream.Context.next_out  := Stream.Buffer.Data (Stream.Buffer.Data'First)'Unchecked_Access;
      Stream.Context.avail_out := Stream.Buffer.Data'Length;
   end Set_Output;

   --  ------------------------------
   --  Read the data stream as much as possible and return the result in a buffer.
   --  The buffer is owned by the stream and need not be released.  The same buffer may
   --  or may not be returned by the next <tt>Read</tt> operation.
   --  A null buffer is returned when the end of the data stream is reached.
   --  ------------------------------
   overriding
   procedure Read (Stream : in out Stream_Type;
                   Buffer : out Babel.Files.Buffers.Buffer_Access) is
      use type Babel.Files.Buffers.Buffer_Access;

      Inbuf  : Babel.Files.Buffers.Buffer_Access;
      Result : Lzma.Base.lzma_ret;
   begin
      Buffer := Stream.Buffer;
      Stream.Context.next_out  := Buffer.Data (Buffer.Data'First)'Unchecked_Access;
      Stream.Context.avail_out := Buffer.Data'Length;
      loop
         --  Read a block of data from the source file.
         if Stream.Context.avail_in = 0 and Stream.Action = Lzma.Base.LZMA_RUN then
            Stream.Input.Read (Inbuf);
            if Inbuf = null then
               Stream.Action := Lzma.Base.LZMA_FINISH;
            else
               Stream.Context.next_in := Inbuf.Data (Inbuf.Data'First)'Unchecked_Access;
               Stream.Context.avail_in := Interfaces.C.size_t (Inbuf.Last - Inbuf.Data'First + 1);
            end if;
         end if;
         Result := Lzma.Base.lzma_code (Stream.Context'Unchecked_Access, Stream.Action);

         --  Write the output data when the buffer is full or we reached the end of stream.
         exit when Stream.Context.avail_out = 0 or Result = Lzma.Base.LZMA_STREAM_END;
         if Result /= Lzma.Base.LZMA_OK then
            Buffer := null;
            return;
         end if;
      end loop;
      if Buffer.Data'Length = Stream.Context.avail_out then
         Buffer := null;
      else
         Buffer.Last :=
           Buffer.Data'Last - Ada.Streams.Stream_Element_Offset (Stream.Context.avail_out);
      end if;
   end Read;

   --  ------------------------------
   --  Write the buffer in the data stream.
   --  ------------------------------
   overriding
   procedure Write (Stream : in out Stream_Type;
                    Buffer : in Babel.Files.Buffers.Buffer_Access) is
      Result : Lzma.Base.lzma_ret;
   begin
      Stream.Context.next_in   := Buffer.Data (Buffer.Data'First)'Unchecked_Access;
      Stream.Context.avail_in  := Interfaces.C.size_t (Buffer.Last - Buffer.Data'First + 1);
      loop
         --  Read a block of data from the source file.
         exit when Stream.Context.avail_in = 0;
         Result := Lzma.Base.lzma_code (Stream.Context'Unchecked_Access, Stream.Action);

         --  Write the output data when the buffer is full or we reached the end of stream.
         if Stream.Context.avail_out = 0 or Result = Lzma.Base.LZMA_STREAM_END then
            Stream.Buffer.Last := Stream.Buffer.Data'Last
              - Ada.Streams.Stream_Element_Offset (Stream.Context.avail_out);
            Stream.Output.Write (Stream.Buffer);
            Stream.Context.avail_out := Stream.Buffer.Data'Length;
            Stream.Context.next_out  :=
              Stream.Buffer.Data (Stream.Buffer.Data'First)'Unchecked_Access;
         end if;
         exit when Result /= Lzma.Base.LZMA_OK;
      end loop;
   end Write;

   --  ------------------------------
   --  Flush the data stream.
   --  ------------------------------
   overriding
   procedure Flush (Stream : in out Stream_Type) is
      Result : Lzma.Base.lzma_ret;
   begin
      Stream.Action := Lzma.Base.LZMA_FINISH;
      Stream.Context.avail_in  := 0;
      loop
         --  Read a block of data from the source file.
         Result := Lzma.Base.lzma_code (Stream.Context'Unchecked_Access, Stream.Action);

         --  Write the output data when the buffer is full or we reached the end of stream.
         if Stream.Context.avail_out = 0 or Result = Lzma.Base.LZMA_STREAM_END then
            Stream.Buffer.Last := Stream.Buffer.Data'Last
              - Ada.Streams.Stream_Element_Offset (Stream.Context.avail_out);
            Stream.Output.Write (Stream.Buffer);
            Stream.Context.avail_out := Stream.Buffer.Data'Length;
            Stream.Context.next_out  :=
              Stream.Buffer.Data (Stream.Buffer.Data'First)'Unchecked_Access;
         end if;
         exit when Result /= Lzma.Base.LZMA_OK;
      end loop;
      Stream.Output.Flush;
   end Flush;

   --  ------------------------------
   --  Close the data stream.
   --  ------------------------------
   overriding
   procedure Close (Stream : in out Stream_Type) is
   begin
      if Stream.Output /= null then
         Stream.Flush;
         Stream.Output.Close;
      end if;

   end Close;

   --  ------------------------------
   --  Prepare to read again the data stream from the beginning.
   --  ------------------------------
   overriding
   procedure Rewind (Stream : in out Stream_Type) is
   begin
      Stream.Action := Lzma.Base.LZMA_RUN;
      Stream.Input.Rewind;
   end Rewind;

   --  ------------------------------
   --  Release the stream buffer and the LZMA stream.
   --  ------------------------------
   overriding
   procedure Finalize (Stream : in out Stream_Type) is
   begin
      Lzma.Base.lzma_end (Stream.Context'Unchecked_Access);
      Babel.Streams.Stream_Type (Stream).Finalize;
   end Finalize;

end Babel.Streams.XZ;

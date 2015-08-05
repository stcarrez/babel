-----------------------------------------------------------------------
--  babel-Streams -- Stream management
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

with Ada.Finalization;
with Babel.Files.Buffers;
package Babel.Streams is

   type Stream_Type is abstract tagged limited private;
   type Stream_Access is access all Stream_Type'Class;

   --  Read the data stream as much as possible and return the result in a buffer.
   --  The buffer is owned by the stream and need not be released.  The same buffer may
   --  or may not be returned by the next <tt>Read</tt> operation.
   --  A null buffer is returned when the end of the data stream is reached.
   procedure Read (Stream : in out Stream_Type;
                   Buffer : out Babel.Files.Buffers.Buffer_Access) is abstract;

   --  Write the buffer in the data stream.
   procedure Write (Stream : in out Stream_Type;
                    Buffer : in Babel.Files.Buffers.Buffer_Access) is abstract;

   --  Flush the data stream.
   procedure Flush (Stream : in out Stream_Type) is null;

   --  Close the data stream.
   procedure Close (Stream : in out Stream_Type) is null;

   --  Prepare to read again the data stream from the beginning.
   procedure Rewind (Stream : in out Stream_Type) is null;

private

   type Stream_Type is abstract limited new Ada.Finalization.Limited_Controlled with null record;

end Babel.Streams;
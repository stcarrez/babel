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

package body Babel.Streams is

   --  ------------------------------
   --  Set the internal buffer that the stream can use.
   --  ------------------------------
   procedure Set_Buffer (Stream : in out Stream_Type;
                         Buffer : in Babel.Files.Buffers.Buffer_Access) is
   begin
      Stream.Buffer := Buffer;
   end Set_Buffer;

   --  ------------------------------
   --  Release the stream buffer if there is one.
   --  ------------------------------
   overriding
   procedure Finalize (Stream : in out Stream_Type) is
      use type Babel.Files.Buffers.Buffer_Access;
   begin
      if Stream.Buffer /= null then
         Babel.Files.Buffers.Release (Stream.Buffer);
      end if;
   end Finalize;

end Babel.Streams;

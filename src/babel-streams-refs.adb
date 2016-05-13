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

package body Babel.Streams.Refs is

   procedure Copy (From : in Stream_Ref;
                   Into : in Stream_Ref) is
      use type Babel.Files.Buffers.Buffer_Access;

      From_Stream : constant Stream_Access := From.Value;
      Into_Stream : constant Stream_Access := Into.Value;
      Buffer      : Babel.Files.Buffers.Buffer_Access;
   begin
      From_Stream.Rewind;
      loop
         From_Stream.Read (Buffer);
         exit when Buffer = null;
         Into_Stream.Write (Buffer);
      end loop;
   end Copy;

end Babel.Streams.Refs;

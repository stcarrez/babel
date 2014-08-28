-----------------------------------------------------------------------
--  babel-files-buffers -- File buffer management
--  Copyright (C) 2014 Stephane.Carrez
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
with Util.Concurrent.Pools;
package Babel.Files.Buffers is

   type Buffer (Max_Size : Ada.Streams.Stream_Element_Offset) is record
      Last   : Ada.Streams.Stream_Element_Offset;
      Data   : Ada.Streams.Stream_Element_Array (0 .. Max_Size);
   end record;

   type Buffer_Access is access all Buffer;

   package Buffer_Pools is new Util.Concurrent.Pools (Element_Type => Buffer_Access);

   type Buffer_Pool_Access is access all Buffer_Pools.Pool;

   --  Create the buffer pool with a number of pre-allocated buffers of the given maximum size.
   procedure Create_Pool (Into  : in out Buffer_Pools.Pool;
                          Size  : in Positive;
                          Count : in Positive);

end Babel.Files.Buffers;

-----------------------------------------------------------------------
--  babel-files-buffers -- File buffer management
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

package body Babel.Files.Buffers is

   --  ------------------------------
   --  Restore the buffer back to the owning pool.
   --  ------------------------------
   procedure Release (Buffer : in out Buffer_Access) is
   begin
      Buffer.Pool.Release (Buffer);
      Buffer := null;
   end Release;

   --  ------------------------------
   --  Create the buffer pool with a number of pre-allocated buffers of the given maximum size.
   --  ------------------------------
   procedure Create_Pool (Into  : in out Buffer_Pools.Pool;
                          Size  : in Positive;
                          Count : in Positive) is
   begin
      Into.Set_Size (Count);
      for I in 1 .. Count loop
         Into.Release (new Buffer (Max_Size => Ada.Streams.Stream_Element_Offset (Size),
                                   Pool     => Into'Unchecked_Access));
      end loop;
   end Create_Pool;

end Babel.Files.Buffers;

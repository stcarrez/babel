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
with Ada.Unchecked_Deallocation;
package body Babel.Files.Buffers is

   --  ------------------------------
   --  Allocate a buffer from the pool.
   --  ------------------------------
   procedure Get_Buffer (Pool   : in out Buffer_Pool;
                         Buffer : out Buffer_Access) is
   begin
      Pool.Pool.Get_Instance (Buffer);
   end Get_Buffer;

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
   procedure Create_Pool (Into  : in out Buffer_Pool;
                          Size  : in Positive;
                          Count : in Positive) is
   begin
      Into.Pool.Set_Size (Count);
      for I in 1 .. Count loop
         Into.Pool.Release (new Buffer (Max_Size => Ada.Streams.Stream_Element_Offset (Size),
                                        Pool     => Into.Pool'Unchecked_Access));
      end loop;
   end Create_Pool;

   --  ------------------------------
   --  Release the buffers allocated for the pool.
   --  ------------------------------
   overriding
   procedure Finalize (Pool : in out Buffer_Pool) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Buffer,
                                        Name   => Buffer_Access);
      Available : Natural;
      Buffer    : Buffer_Access;
   begin
      loop
         Pool.Pool.Get_Available (Available);
         exit when Available = 0;
         Pool.Pool.Get_Instance (Buffer);
         Free (Buffer);
      end loop;
   end Finalize;

end Babel.Files.Buffers;

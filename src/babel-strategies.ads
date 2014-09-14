-----------------------------------------------------------------------
--  babel-strategies -- Strategies to backup files
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

with Babel.Files;
with Babel.Files.Queues;
with Babel.Files.Buffers;
with Babel.Stores;
with Babel.Filters;
package Babel.Strategies is

   type Strategy_Type is abstract tagged limited private;
   type Strategy_Type_Access is access all Strategy_Type'Class;

   --  Allocate a buffer to read the file content.
   function Allocate_Buffer (Strategy : in Strategy_Type) return Babel.Files.Buffers.Buffer_Access;

   --  Release the buffer that was allocated by Allocate_Buffer.
   procedure Release_Buffer (Strategy : in Strategy_Type;
                             Buffer   : in out Babel.Files.Buffers.Buffer_Access);

   --  Returns true if there is a directory that must be processed by the current strategy.
   function Has_Directory (Strategy : in Strategy_Type) return Boolean is abstract;

   --  Get the next directory that must be processed by the strategy.
   procedure Peek_Directory (Strategy  : in out Strategy_Type;
                             Directory : out Babel.Files.Directory_Type) is abstract;

   --  Scan the directory
   procedure Scan (Strategy  : in out Strategy_Type;
                   Directory : in Babel.Files.Directory_Type;
                   Container : in out Babel.Files.File_Container'Class);

   --  Scan the directories which are defined in the directory queue and
   --  use the file container to scan the files and directories.
   procedure Scan (Strategy  : in out Strategy_Type;
                   Queue     : in out Babel.Files.Queues.Directory_Queue;
                   Container : in out Babel.Files.File_Container'Class);

   --  Read the file from the read store into the local buffer.
   procedure Read_File (Strategy : in Strategy_Type;
                        File     : in Babel.Files.File_Type;
                        Into     : in Babel.Files.Buffers.Buffer_Access);

   --  Write the file from the local buffer into the write store.
   procedure Write_File (Strategy : in Strategy_Type;
                         File     : in Babel.Files.File_Type;
                         Content  : in Babel.Files.Buffers.Buffer_Access);

   --  Backup the file from the local buffer into the write store.
   procedure Backup_File (Strategy : in Strategy_Type;
                          File     : in Babel.Files.File_Type;
                          Content  : in out Babel.Files.Buffers.Buffer_Access);

   procedure Execute (Strategy : in out Strategy_Type) is abstract;

   --  Set the file filters that will be used when scanning the read store.
   procedure Set_Filters (Strategy : in out Strategy_Type;
                          Filters  : in Babel.Filters.Filter_Type_Access);

   --  Set the read and write stores that the strategy will use.
   procedure Set_Stores (Strategy : in out Strategy_Type;
                         Read     : in Babel.Stores.Store_Type_Access;
                         Write    : in Babel.Stores.Store_Type_Access);

   --  Set the buffer pool to be used by Allocate_Buffer.
   procedure Set_Buffers (Strategy : in out Strategy_Type;
                          Buffers  : in Babel.Files.Buffers.Buffer_Pool_Access);

private

   type Strategy_Type is abstract tagged limited record
      Read_Store  : Babel.Stores.Store_Type_Access;
      Write_Store : Babel.Stores.Store_Type_Access;
      Filters     : Babel.Filters.Filter_Type_Access;
      Buffers     : Babel.Files.Buffers.Buffer_Pool_Access;
   end record;

end Babel.Strategies;

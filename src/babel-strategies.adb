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

with Util.Log.Loggers;
with Babel.Files;
with Babel.Files.Buffers;
with Babel.Files.Lifecycles;
with Babel.Stores;
package body Babel.Strategies is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Babel.Strategies");

   --  ------------------------------
   --  Allocate a buffer to read the file content.
   --  ------------------------------
   function Allocate_Buffer (Strategy : in Strategy_Type) return Babel.Files.Buffers.Buffer_Access is
      Result : Babel.Files.Buffers.Buffer_Access;
   begin
      Strategy.Buffers.Get_Buffer (Result);
      return Result;
   end Allocate_Buffer;

   --  ------------------------------
   --  Release the buffer that was allocated by Allocate_Buffer.
   --  ------------------------------
   procedure Release_Buffer (Strategy : in Strategy_Type;
                             Buffer   : in out Babel.Files.Buffers.Buffer_Access) is
   begin
      Babel.Files.Buffers.Release (Buffer);
      Buffer := null;
   end Release_Buffer;

   --  ------------------------------
   --  Set the buffer pool to be used by Allocate_Buffer.
   --  ------------------------------
   procedure Set_Buffers (Strategy : in out Strategy_Type;
                          Buffers  : in Babel.Files.Buffers.Buffer_Pool_Access) is
   begin
      Strategy.Buffers := Buffers;
   end Set_Buffers;

   --  Read the file from the read store into the local buffer.
   procedure Read_File (Strategy : in Strategy_Type;
                        File     : in Babel.Files.File_Type;
                        Stream   : in out Babel.Streams.Refs.Stream_Ref) is
      Path : constant String := Babel.Files.Get_Path (File);
   begin
      Strategy.Read_Store.Read_File (Path, Stream);
   end Read_File;

   --  Write the file from the local buffer into the write store.
   procedure Write_File (Strategy : in Strategy_Type;
                         File     : in Babel.Files.File_Type;
                         Stream   : in Babel.Streams.Refs.Stream_Ref) is
      Path : constant String := Babel.Files.Get_Path (File);
   begin
      Strategy.Write_Store.Write_File (Path, Stream, Babel.Files.Get_Mode (File));
   end Write_File;

   --  Backup the file from the local buffer into the write store.
   procedure Backup_File (Strategy : in out Strategy_Type;
                          File     : in Babel.Files.File_Type;
                          Stream   : in Babel.Streams.Refs.Stream_Ref) is
   begin
      Strategy.Database.Insert (File);
      if Strategy.Listeners /= null then
         if Babel.Files.Is_New (File) then
            Babel.Files.Lifecycles.Notify_Create (Strategy.Listeners.all, File);
         else
            Babel.Files.Lifecycles.Notify_Update (Strategy.Listeners.all, File);
         end if;
      end if;
      Strategy.Write_File (File, Stream);
   end Backup_File;

   --  Scan the directory
   procedure Scan (Strategy  : in out Strategy_Type;
                   Directory : in Babel.Files.Directory_Type;
                   Container : in out Babel.Files.File_Container'Class) is
      Path : constant String := Babel.Files.Get_Path (Directory);
   begin
      Strategy.Read_Store.Scan (Path, Container, Strategy.Filters.all);
   end Scan;

   --  ------------------------------
   --  Scan the directories which are defined in the directory queue and
   --  use the file container to scan the files and directories.
   --  ------------------------------
   procedure Scan (Strategy  : in out Strategy_Type;
                   Queue     : in out Babel.Files.Queues.Directory_Queue;
                   Container : in out Babel.Files.File_Container'Class) is

      procedure Append_Directory (Directory : in Babel.Files.Directory_Type) is
      begin
         Babel.Files.Queues.Add_Directory (Queue, Directory);
      end Append_Directory;

      Dir : Babel.Files.Directory_Type;
   begin
      while Babel.Files.Queues.Has_Directory (Queue) loop
         Babel.Files.Queues.Peek_Directory (Queue, Dir);
         Container.Set_Directory (Dir);
         Strategy_Type'Class (Strategy).Scan (Dir, Container);
         Container.Each_Directory (Append_Directory'Access);
      end loop;
   end Scan;

   --  ------------------------------
   --  Set the file filters that will be used when scanning the read store.
   --  ------------------------------
   procedure Set_Filters (Strategy : in out Strategy_Type;
                          Filters  : in Babel.Filters.Filter_Type_Access) is
   begin
      Strategy.Filters := Filters;
   end Set_Filters;

   --  ------------------------------
   --  Set the read and write stores that the strategy will use.
   --  ------------------------------
   procedure Set_Stores (Strategy : in out Strategy_Type;
                         Read     : in Babel.Stores.Store_Type_Access;
                         Write    : in Babel.Stores.Store_Type_Access) is
   begin
      Strategy.Read_Store  := Read;
      Strategy.Write_Store := Write;
   end Set_Stores;

   --  ------------------------------
   --  Set the listeners to inform about changes.
   --  ------------------------------
   procedure Set_Listeners (Strategy  : in out Strategy_Type;
                            Listeners : access Util.Listeners.List) is
   begin
      Strategy.Listeners := Listeners;
   end Set_Listeners;

   --  ------------------------------
   --  Set the database for use by the strategy.
   --  ------------------------------
   procedure Set_Database (Strategy : in out Strategy_Type;
                           Database : in Babel.Base.Database_Access) is
   begin
      --        Strategy.Database := Database;
      null;
   end Set_Database;

end Babel.Strategies;

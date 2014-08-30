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
pragma Ada_2012;

with Util.Log.Loggers;
with Util.Encoders.Base16;
with Babel.Files;
with Babel.Files.Buffers;
with Babel.Stores;
package body Babel.Strategies is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Babel.Strategies");

   Hex_Encoder : Util.Encoders.Base16.Encoder;

   --  ------------------------------
   --  Allocate a buffer to read the file content.
   --  ------------------------------
   function Allocate_Buffer (Strategy : in Strategy_Type) return Babel.Files.Buffers.Buffer_Access is
      Result : Babel.Files.Buffers.Buffer_Access;
   begin
      Strategy.Buffers.Get_Instance (Result);
      return Result;
   end Allocate_Buffer;

   --  ------------------------------
   --  Release the buffer that was allocated by Allocate_Buffer.
   --  ------------------------------
   procedure Release_Buffer (Strategy : in Strategy_Type;
                             Buffer   : in out Babel.Files.Buffers.Buffer_Access) is
   begin
      Strategy.Buffers.Release (Buffer);
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
                        File     : in Babel.Files.File;
                        Into     : in Babel.Files.Buffers.Buffer_Access) is
      Path : constant String := Babel.Files.Get_Path (File);
   begin
      Strategy.Read_Store.Read (Path, Into.all);
   end Read_File;

   --  Write the file from the local buffer into the write store.
   procedure Write_File (Strategy : in Strategy_Type;
                         File     : in Babel.Files.File;
                         Content  : in Babel.Files.Buffers.Buffer_Access) is
      Path : constant String := Babel.Files.Get_Path (File);
   begin
      Strategy.Write_Store.Write (Path, Content.all);
   end Write_File;

   procedure Print_Sha (File : in Babel.Files.File) is
      Sha : constant String := Hex_Encoder.Transform (File.SHA1);
   begin
      Log.Info (Babel.Files.Get_Path (File) & " => " & Sha);
   end Print_Sha;

   --  Backup the file from the local buffer into the write store.
   procedure Backup_File (Strategy : in Strategy_Type;
                          File     : in Babel.Files.File;
                          Content  : in out Babel.Files.Buffers.Buffer_Access) is
   begin
      Print_Sha (File);
      Strategy.Write_File (File, Content);
      Strategy.Release_Buffer (Content);
   end Backup_File;

   procedure Scan (Strategy : in out Strategy_Type;
                   Path     : in String) is
   begin
      Strategy.Read_Store.Scan (Path, Strategy, Strategy.Filters.all);
      while Strategy_Type'Class (Strategy).Has_Directory loop
         declare
            Path : constant String := Strategy_Type'Class (Strategy).Peek_Directory;
         begin
            Strategy.Read_Store.Scan (Path, Strategy, Strategy.Filters.all);
         end;
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

end Babel.Strategies;

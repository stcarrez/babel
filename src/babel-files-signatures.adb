-----------------------------------------------------------------------
--  babel-files-signatures -- Signatures calculation
--  Copyright (C) 2014 , 2015, 2016 Stephane.Carrez
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
with Ada.Text_IO;
package body Babel.Files.Signatures is

   --  ------------------------------
   --  Compute the SHA1 signature of the data stored in the buffer.
   --  ------------------------------
   procedure Sha1 (Buffer : in Babel.Files.Buffers.Buffer;
                   Result : out Util.Encoders.SHA1.Hash_Array) is
      Ctx  : Util.Encoders.SHA1.Context;
   begin
      Util.Encoders.SHA1.Update (Ctx, Buffer.Data (Buffer.Data'First .. Buffer.Last));
      Util.Encoders.SHA1.Finish (Ctx, Result);
   end Sha1;

   --  ------------------------------
   --  Compute the SHA1 signature of the file stream.
   --  ------------------------------
   procedure Sha1 (Stream : in Babel.Streams.Refs.Stream_Ref;
                   Result : out Util.Encoders.SHA1.Hash_Array) is
      use type Babel.Files.Buffers.Buffer_Access;

      From_Stream : constant Babel.Streams.Stream_Access := Stream.Value;
      Buffer      : Babel.Files.Buffers.Buffer_Access;
      Ctx         : Util.Encoders.SHA1.Context;
   begin
      From_Stream.Rewind;
      loop
         From_Stream.Read (Buffer);
         exit when Buffer = null;
         Util.Encoders.SHA1.Update (Ctx, Buffer.Data (Buffer.Data'First .. Buffer.Last));
      end loop;
      Util.Encoders.SHA1.Finish (Ctx, Result);
   end Sha1;

   --  ------------------------------
   --  Write the SHA1 checksum for the files stored in the map.
   --  ------------------------------
   procedure Save_Checksum (Path  : in String;
                            Files : in Babel.Files.Maps.File_Map) is
      Checksum : Ada.Text_IO.File_Type;

      procedure Write_Checksum (Position : in Babel.Files.Maps.File_Cursor) is
         File : constant Babel.Files.File_Type := Babel.Files.Maps.File_Maps.Element (Position);
         SHA1 : constant String := Babel.Files.Get_SHA1 (File);
         Path : constant String := Babel.Files.Get_Path (File);
      begin
         Ada.Text_IO.Put (Checksum, Path);
         Ada.Text_IO.Put (Checksum, ": ");
         Ada.Text_IO.Put_Line (Checksum, SHA1);
      end Write_Checksum;

   begin
      Ada.Text_IO.Create (File => Checksum, Name => Path);
      Files.Iterate (Write_Checksum'Access);
      Ada.Text_IO.Close (File => Checksum);
   end Save_Checksum;

end Babel.Files.Signatures;

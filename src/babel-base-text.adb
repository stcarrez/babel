-----------------------------------------------------------------------
--  babel-base -- Database for files
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
with Util.Dates.ISO8601;
with Babel.Files.Sets;
with Babel.Files.Lifecycles;
with Ada.Text_IO;
package body Babel.Base.Text is

   --  ------------------------------
   --  Insert the file in the database.
   --  ------------------------------
   overriding
   procedure Insert (Into : in out Text_Database;
                     File : in Babel.Files.File_Type) is
   begin
      Into.Files.Insert (File);
   end Insert;

   --  ------------------------------
   --  Write the SHA1 checksum for the files stored in the map.
   --  ------------------------------
   procedure Save (Database : in Text_Database;
                   Path     : in String) is
      Checksum : Ada.Text_IO.File_Type;

      procedure Write_Checksum (Position : in Babel.Files.Sets.File_Cursor) is
         File : constant Babel.Files.File_Type := Babel.Files.Sets.File_Sets.Element (Position);
         SHA1 : constant String := Babel.Files.Get_SHA1 (File);
         Path : constant String := Babel.Files.Get_Path (File);
      begin
         Ada.Text_IO.Put (Checksum, SHA1);
         Ada.Text_IO.Put (Checksum, Babel.Files.Uid_Type'Image (Babel.Files.Get_User (File)));
         Ada.Text_IO.Put (Checksum, Babel.Files.Gid_Type'Image (Babel.Files.Get_Group (File)));
         Ada.Text_IO.Put (Checksum, " ");
         Ada.Text_IO.Put (Checksum, Util.Dates.ISO8601.Image (Babel.Files.Get_Date (File)));
         Ada.Text_IO.Put (Checksum, Babel.Files.File_Size'Image (Babel.Files.Get_Size (File)));
         Ada.Text_IO.Put (Checksum, " ");
         Ada.Text_IO.Put (Checksum, Path);
         Ada.Text_IO.New_Line (Checksum);
      end Write_Checksum;

   begin
      Ada.Text_IO.Create (File => Checksum, Name => Path);
      Database.Files.Iterate (Write_Checksum'Access);
      Ada.Text_IO.Close (File => Checksum);
   end Save;

end Babel.Base.Text;

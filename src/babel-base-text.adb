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
with Ada.Streams;
with Ada.Calendar;
with Ada.Exceptions;

with Util.Dates.ISO8601;
with Util.Files;
with Util.Strings;
with Util.Encoders.Base16;
with Util.Encoders.SHA1;
with Util.Log.Loggers;
with Babel.Files.Sets;
with Babel.Files.Maps;
with Babel.Files.Lifecycles;
with Ada.Text_IO;
package body Babel.Base.Text is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Babel.Base.Text");

   --  ------------------------------
   --  Insert the file in the database.
   --  ------------------------------
   overriding
   procedure Insert (Into : in out Text_Database;
                     File : in Babel.Files.File_Type) is
   begin
      Into.Files.Insert (File);
   end Insert;

   overriding
   procedure Iterate (From    : in Text_Database;
                      Process : not null access procedure (File : in Babel.Files.File_Type)) is

      procedure Process_One (Pos : in Babel.Files.Sets.File_Cursor) is
      begin
         Process (Babel.Files.Sets.File_Sets.Element (Pos));
      end Process_One;

   begin
      From.Files.Iterate (Process_One'Access);
   end Iterate;

   --  ------------------------------
   --  Save the database file description in the file.
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
         Ada.Text_IO.Put (Checksum, Babel.Uid_Type'Image (Babel.Files.Get_User (File)));
         Ada.Text_IO.Put (Checksum, Babel.Gid_Type'Image (Babel.Files.Get_Group (File)));
         Ada.Text_IO.Put (Checksum, " ");
         Ada.Text_IO.Put (Checksum, Util.Dates.ISO8601.Image (Babel.Files.Get_Date (File)));
         Ada.Text_IO.Put (Checksum, Babel.Files.File_Size'Image (Babel.Files.Get_Size (File)));
         Ada.Text_IO.Put (Checksum, " ");
         Ada.Text_IO.Put (Checksum, Path);
         Ada.Text_IO.New_Line (Checksum);
      end Write_Checksum;

   begin
      Log.Info ("Save text database {0}", Path);

      Ada.Text_IO.Create (File => Checksum, Name => Path);
      Database.Files.Iterate (Write_Checksum'Access);
      Ada.Text_IO.Close (File => Checksum);
   end Save;

   --  ------------------------------
   --  Load the database file description from the file.
   --  ------------------------------
   procedure Load (Database : in out Text_Database;
                   Path     : in String) is
      Checksum    : Ada.Text_IO.File_Type;
      Dirs        : Babel.Files.Maps.Directory_Map;
      Files       : Babel.Files.Maps.File_Map;
      Hex_Decoder : Util.Encoders.Base16.Decoder;
      Line_Number : Natural := 0;

      procedure Read_Line (Line : in String) is
         File      : Babel.Files.File_Type;
         Pos       : Natural;
         First     : Natural;
         Sign      : Util.Encoders.SHA1.Hash_Array;
         Sign_Size : Ada.Streams.Stream_Element_Offset;
         User      : Uid_Type;
         Group     : Gid_Type;
         Date      : Ada.Calendar.Time;
         Size      : Babel.Files.File_Size;
      begin
         Line_Number := Line_Number + 1;
         Pos := Util.Strings.Index (Line, ' ');
         if Pos = 0 then
            return;
         end if;
         Hex_Decoder.Transform (Line (Line'First .. Pos - 1), Sign, Sign_Size);

         --  Extract the user ID.
         First := Pos + 1;
         Pos := Util.Strings.Index (Line, ' ', First);
         if Pos = 0 then
            return;
         end if;
         User := Uid_Type'Value (Line (First .. Pos - 1));

         --  Extract the group ID.
         First := Pos + 1;
         Pos := Util.Strings.Index (Line, ' ', First);
         if Pos = 0 then
            return;
         end if;
         Group := Uid_Type'Value (Line (First .. Pos - 1));

         --  Extract the file date.
         First := Pos + 1;
         Pos := Util.Strings.Index (Line, ' ', First);
         if Pos = 0 then
            return;
         end if;
         Date := Util.Dates.ISO8601.Value (Line (First .. Pos - 1));

         --  Extract the file size.
         First := Pos + 1;
         Pos := Util.Strings.Index (Line, ' ', First);
         if Pos = 0 then
            return;
         end if;
         Size := Babel.Files.File_Size'Value (Line (First .. Pos - 1));

         Babel.Files.Maps.Add_File (Dirs, Files, Line (Pos + 1 .. Line'Last), File);
         Babel.Files.Set_Owner (File, User, Group);
         Babel.Files.Set_Size (File, Size);
         Babel.Files.Set_Signature (File, Sign);
         Babel.Files.Set_Date (File, Date);
         Database.Insert (File);

      exception
         when E : others =>
            Log.Error ("{0}:{1}: Error: {2}: {3}: " & Line, Path, Natural'Image (Line_Number),
                       Ada.Exceptions.Exception_Message (E));
      end Read_Line;

   begin
      Log.Info ("Load text database {0}", Path);

      Util.Files.Read_File (Path, Read_Line'Access);
   end Load;

end Babel.Base.Text;

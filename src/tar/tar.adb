-----------------------------------------------------------------------
--  tar -- TAR file
--  Copyright (C) 2015 Stephane.Carrez
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

package body Tar is

   use type Ada.Streams.Stream_Element_Offset;
   use type Ada.Streams.Stream_Element;

   BLOCK_SIZE : constant Ada.Streams.Stream_Element_Offset := 512;

   --  ------------------------------
   --  Open the tar file for reading and use the given input stream to read the tar blocks.
   --  ------------------------------
   procedure Open (File  : in out File_Type;
                   Input : in Util.Streams.Input_Stream_Access) is
   begin
      File.Input := Input;
      File.Remain := 0;
      File.Has_Current := False;
      File.Next;
   end Open;

   --  ------------------------------
   --  Returns true if the tar file contains another file entry.
   --  ------------------------------
   function Has_Element (File : in File_Type) return Boolean is
   begin
      return File.Has_Current;
   end Has_Element;

   procedure Get_String (From   : in Ada.Streams.Stream_Element_Array;
                         Pos    : in Ada.Streams.Stream_Element_Offset;
                         Length : in Ada.Streams.Stream_Element_Count;
                         Into   : out Ada.Strings.Unbounded.Unbounded_String) is
      Item     : String (1 .. Positive (Length));
      Item_Pos : Natural := 0;
      C        : Ada.Streams.Stream_Element;
   begin
      for I in Pos .. Pos + Length - 1 loop
         C := From (I);
         exit when C = 0;
         Item_Pos := Item_Pos + 1;
         Item (Item_Pos) := Character'Val (C);
      end loop;
      Into := Ada.Strings.Unbounded.To_Unbounded_String (Item (Item'First .. Item_Pos));
   end Get_String;

   function Get_Number (From   : in Ada.Streams.Stream_Element_Array;
                        Pos    : in Ada.Streams.Stream_Element_Offset;
                        Length : in Ada.Streams.Stream_Element_Count) return Long_Long_Integer is
      Result : Long_Long_Integer := 0;
      C      : Character;
   begin
      for I in Pos .. Pos + Length - 1 loop
         exit when From (I) = 0;
         Result := Result * 8;
         C := Character'Val (From (I));
         if C >= '0' and C <= '7' then
            Result := Result + Character'Pos (C) - Character'Pos ('0');
         end if;
      end loop;
      return Result;
   end Get_Number;

   function Is_Zero (Buf : in Ada.Streams.Stream_Element_Array) return Boolean is
   begin
      for I in Buf'Range loop
         if Buf (I) /= 0 then
            return False;
         end if;
      end loop;
      return True;
   end Is_Zero;

   --  ------------------------------
   --  Read the next file entry in the tar file.
   --  ------------------------------
   procedure Next (File : in out File_Type) is
      use Util.Systems.Types;

      Buf  : Ada.Streams.Stream_Element_Array (0 .. BLOCK_SIZE - 1);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      File.Has_Current := False;
      while File.Remain > 0 loop
         File.Read (Buf, Last);
         exit when Last = 0;
      end loop;
      if File.Padding > 0 then
         File.Input.Read (Buf (1 .. File.Padding), Last);
      end if;
      File.Input.Read (Buf, Last);
      if Last = BLOCK_SIZE and (Buf (Buf'First) /= 0 or else not Is_Zero (Buf)) then
         File.Has_Current := True;
         Get_String (Buf, 0, 100, File.Current.Name);
         File.Current.Mode := mode_t (Get_Number (Buf, 100, 8));
         File.Current.Uid := uid_t (Get_Number (Buf, 108, 8));
         File.Current.Gid := gid_t (Get_Number (Buf, 116, 8));
         File.Current.Size := off_t (Get_Number (Buf, 124, 12));
         File.Current.Mtime := Get_Number (Buf, 136, 12);
         File.Current.Chksum := Natural (Get_Number (Buf, 148, 8));
         --  typeflag/156
         Get_String (Buf, 157, 100, File.Current.Linkname);
         Get_String (Buf, 265, 32, File.Current.Uname);
         Get_String (Buf, 297, 32, File.Current.Gname);
         File.Current.Devmajor := Get_Number (Buf, 329, 8);
         File.Current.Devminor := Get_Number (Buf, 337, 8);
         Get_String (Buf, 345, 155, File.Current.Prefix);
         File.Remain := Ada.Streams.Stream_Element_Count (File.Current.Size);
         File.Padding := (BLOCK_SIZE - (File.Remain mod BLOCK_SIZE)) mod BLOCK_SIZE;
      end if;
   end Next;

   --  ------------------------------
   --  Get the current file entry information.
   --  ------------------------------
   function Element (File : in File_Type) return File_Info_Type is
   begin
      return File.Current;
   end Element;

   --  ------------------------------
   --  Read the data for the current file entry.
   --  ------------------------------
   procedure Read (File   : in out File_Type;
                   Into   : out Ada.Streams.Stream_Element_Array;
                   Last   : out Ada.Streams.Stream_Element_Offset) is
   begin
      if File.Remain = 0 then
         Last := 0;
      elsif File.Remain >= Into'Length then
         File.Input.Read (Into, Last);
         File.Remain := File.Remain - Last;
      else
         File.Input.Read (Into (Into'First .. Into'First + File.Remain - 1), Last);
         File.Remain := File.Remain - Last;
      end if;
   end Read;

end Tar;

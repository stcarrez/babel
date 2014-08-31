--  bkp-files -- File and directories
-----------------------------------------------------------------------
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
with Ada.Strings.Unbounded;
with Ada.Directories;
with Ada.Containers.Vectors;
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Util.Log.Loggers;
with Util.Files;
with Interfaces.C;
package body Babel.Files is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Bkp.Files");

   --  ------------------------------
   --  Allocate a File_Type entry with the given name for the directory.
   --  ------------------------------
   function Allocate (Name : in String;
                      Dir  : in Directory_Type) return File_Type is
      Result : constant File_Type := new File '(Len    => Name'Length,
                                                Id     => NO_IDENTIFIER,
                                                Dir    => Dir,
                                                Name   => Name,
                                                others => <>);
   begin
      return Result;
   end Allocate;

   --  ------------------------------
   --  Return true if the file was modified and need a backup.
   --  ------------------------------
   function Is_Modified (Element : in File_Type) return Boolean is
      use type ADO.Identifier;
   begin
      if Element.Id = NO_IDENTIFIER then
         return True;
      elsif (Element.Status and FILE_MODIFIED) /= 0 then
         return True;
      else
         return False;
      end if;
   end Is_Modified;

   --  ------------------------------
   --  Set the file as modified.
   --  ------------------------------
   procedure Set_Modified (Element : in File_Type) is
   begin
      Element.Status := Element.Status or FILE_MODIFIED;
   end Set_Modified;

   --  ------------------------------
   --  Return the path for the file.
   --  ------------------------------
   function Get_Path (Element : in File) return String is
   begin
      return Util.Files.Compose (Ada.Strings.Unbounded.To_String (Element.Path),
                                 Ada.Strings.Unbounded.To_String (Element.Name));
   end Get_Path;

   overriding
   procedure Add_File (Into    : in out File_Queue;
                       Path    : in String;
                       Element : in File) is
   begin
      Into.Queue.Enqueue (Element);
   end Add_File;

   overriding
   procedure Add_Directory (Into : in out File_Queue;
                            Path : in String;
                            Name : in String) is
   begin
      Into.Directories.Append (Util.Files.Compose (Path, Name));
   end Add_Directory;

   procedure Compute_Sha1 (Path : in String;
                           Into : in out File) is
      use Ada.Strings.Unbounded;
      use type Ada.Streams.Stream_Element_Offset;

      File : Ada.Streams.Stream_IO.File_Type;
      Name : constant String := Path & "/" & To_String (Into.Name);
      Last : Ada.Streams.Stream_Element_Offset;
      Buf  : Ada.Streams.Stream_Element_Array (0 .. 32_767);
      Ctx  : Util.Encoders.SHA1.Context;
   begin
      Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Name);
      loop
         Ada.Streams.Stream_IO.Read (File, Buf, Last);
         Util.Encoders.SHA1.Update (Ctx, Buf (Buf'First .. Last));
         exit when Last /= Buf'Last;
      end loop;
      Ada.Streams.Stream_IO.Close (File);
      Util.Encoders.SHA1.Finish (Ctx, Into.SHA1);
   end Compute_Sha1;

   procedure Iterate_Files (Path   : in String;
                            Dir    : in out Directory;
                            Depth  : in Natural;
                            Update : access procedure (P : in String; F : in out File)) is
      use Ada.Strings.Unbounded;

      Iter : File_Vectors.Cursor := Dir.Files.First;

      procedure Iterate_File (Item : in out File) is
      begin
         Update (Path, Item);
      end Iterate_File;

      procedure Iterate_Child (Item : in out Directory) is
      begin
         Iterate_Files (Path & "/" & To_String (Item.Name), Item, Depth - 1, Update);
      end Iterate_Child;

   begin
      while File_Vectors.Has_Element (Iter) loop
         Dir.Files.Update_Element (Iter, Iterate_File'Access);
         File_Vectors.Next (Iter);
      end loop;
      if Depth > 0 and then Dir.Children /= null then
         declare
            Dir_Iter : Directory_Vectors.Cursor := Dir.Children.First;
         begin
            while Directory_Vectors.Has_Element (Dir_Iter) loop
               Dir.Children.Update_Element (Dir_Iter, Iterate_Child'Access);
               Directory_Vectors.Next (Dir_Iter);
            end loop;
         end;
      end if;
   end Iterate_Files;

   procedure Scan_Files (Path : in String;
                         Into : in out Directory) is
      Iter : File_Vectors.Cursor := Into.Files.First;

      procedure Compute_Sha1 (Item : in out File) is
      begin
         Compute_Sha1 (Path, Item);
      end Compute_Sha1;

   begin
      while File_Vectors.Has_Element (Iter) loop
         Into.Files.Update_Element (Iter, Compute_Sha1'Access);
         File_Vectors.Next (Iter);
      end loop;
   end Scan_Files;

   procedure Scan_Children (Path : in String;
                            Into : in out Directory) is
      procedure Update (Dir : in out Directory) is
         use Ada.Strings.Unbounded;
         use type Ada.Directories.File_Size;
      begin
         Scan (Path & "/" & To_String (Dir.Name), Dir);
         Into.Tot_Files := Into.Tot_Files + Dir.Tot_Files;
         Into.Tot_Size  := Into.Tot_Size + Dir.Tot_Size;
         Into.Tot_Dirs  := Into.Tot_Dirs + Dir.Tot_Dirs;
      end Update;

   begin
      if Into.Children /= null then
         declare
            Iter : Directory_Vectors.Cursor := Into.Children.First;
         begin
            while Directory_Vectors.Has_Element (Iter) loop
               Into.Children.Update_Element (Iter, Update'Access);
               Directory_Vectors.Next (Iter);
            end loop;
         end;
      end if;
   end Scan_Children;

   procedure Scan (Path : in String;
                   Into : in out Directory) is
      use Ada.Directories;

      Filter : constant Ada.Directories.Filter_Type := (Ordinary_File => True,
                                                        Ada.Directories.Directory => True,
                                                        Special_File => False);
      Search   : Search_Type;
      Ent      : Directory_Entry_Type;
      New_File : File;
      Child    : Directory;
   begin
      Into.Tot_Size  := 0;
      Into.Tot_Files := 0;
      Into.Tot_Dirs  := 0;
      Start_Search (Search, Directory => Path, Pattern => "*", Filter => Filter);
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Ent);
         declare
            Name : constant String    := Simple_Name (Ent);
            Kind : constant File_Kind := Ada.Directories.Kind (Ent);
         begin
            if Kind = Ordinary_File then
               New_File.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
               begin
                  New_File.Size := Ada.Directories.Size (Ent);
               exception
                  when Constraint_Error =>
                     New_File.Size := Ada.Directories.File_Size (Interfaces.Unsigned_32'Last);
               end;
               Into.Files.Append (New_File);
               if New_File.Size > 0 then
                  begin
                     Into.Tot_Size  := Into.Tot_Size + New_File.Size;
                  exception
                     when Constraint_Error =>
                        Ada.Text_IO.Put_Line ("Size: " & Ada.Directories.File_Size'Image (New_File.Size)
                                              & " for " & Path & "/" & Name);
                        Ada.Text_IO.Put_Line ("Size: " & Ada.Directories.File_Size'Image (Into.Tot_Size));

                  end;
               end if;
               Into.Tot_Files := Into.Tot_Files + 1;
            elsif Name /= "." and Name /= ".." then
               if Into.Children = null then
                  Into.Children := new Directory_Vector;
               end if;
               Child.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
               Into.Children.Append (Child);
               Into.Tot_Dirs := Into.Tot_Dirs + 1;
            end if;
         end;
      end loop;
      Scan_Children (Path, Into);
      Scan_Files (Path, Into);

   exception
      when E : others =>
            Log.Error ("Exception ", E);
   end Scan;

end Babel.Files;

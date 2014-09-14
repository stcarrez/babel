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

with Util.Log.Loggers;
with Util.Files;
with Util.Encoders.Base16;
package body Babel.Files is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Bkp.Files");

   Hex_Encoder : Util.Encoders.Base16.Encoder;

   --  ------------------------------
   --  Compare two files on their name and directory.
   --  ------------------------------
   function "<" (Left, Right : in File_Type) return Boolean is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      if Left = NO_FILE then
         return False;
      elsif Right = NO_FILE then
         return True;
      elsif Left.Dir = Right.Dir then
         return Left.Name < Right.Name;
      elsif Left.Dir = NO_DIRECTORY then
         return True;
      elsif Right.Dir = NO_DIRECTORY then
         return False;
      else
         return Left.Dir.Path < Right.Dir.Path;
      end if;
   end "<";

   --  ------------------------------
   --  Allocate a File_Type entry with the given name for the directory.
   --  ------------------------------
   function Allocate (Name : in String;
                      Dir  : in Directory_Type) return File_Type is
      use Ada.Strings.Unbounded;
      Result : constant File_Type := new File '(Len    => Name'Length,
                                                Id     => NO_IDENTIFIER,
                                                Dir    => Dir,
                                                Name   => Name,
                                                others => <>);
   begin
      return Result;
   end Allocate;

   --  ------------------------------
   --  Allocate a Directory_Type entry with the given name for the directory.
   --  ------------------------------
   function Allocate (Name : in String;
                      Dir  : in Directory_Type) return Directory_Type is
      use Ada.Strings.Unbounded;

      Result : constant Directory_Type := new Directory '(Len    => Name'Length,
                                                          Id     => NO_IDENTIFIER,
                                                          Parent => Dir,
                                                          Name   => Name,
                                                          others => <>);
   begin
      if Dir /= null then
         Result.Path := To_Unbounded_String
           (Util.Files.Compose (To_String (Dir.Path), Name));
      else
         Result.Path := To_Unbounded_String (Name);
      end if;
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
   --  Set the SHA1 signature that was computed for this file.
   --  If the computed signature is different from the current signature,
   --  the FILE_MODIFIED flag is set on the file.  The FILE_HAS_SHA1 flag
   --  is set on the file.
   --  ------------------------------
   procedure Set_Signature (Element   : in File_Type;
                            Signature : in Util.Encoders.SHA1.Hash_Array) is
      use type Util.Encoders.SHA1.Hash_Array;
   begin
      if (Element.Status and FILE_HAS_SHA1) /= 0 and then Element.SHA1 /= Signature then
         Element.Status := Element.Status or FILE_MODIFIED;
      end if;
      Element.Status := Element.Status or FILE_HAS_SHA1;
      Element.SHA1   := Signature;
   end Set_Signature;

   --  ------------------------------
   --  Set the file size.  If the new size is different, the FILE_MODIFIED
   --  flag is set on the file.
   --  ------------------------------
   procedure Set_Size (Element : in File_Type;
                       Size    : in File_Size) is
   begin
      if Element.Size /= Size then
         Element.Size := Size;
         Element.Status := Element.Status or FILE_MODIFIED;
      end if;
   end Set_Size;

   --  ------------------------------
   --  Return the path for the file.
   --  ------------------------------
   function Get_Path (Element : in File_Type) return String is
   begin
      if Element.Dir = null then
         return Element.Name;
      else
         return Util.Files.Compose (Get_Path (Element.Dir), Element.Name);
      end if;
   end Get_Path;

   --  ------------------------------
   --  Return the path for the directory.
   --  ------------------------------
   function Get_Path (Element : in Directory_Type) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Element.Path);
   end Get_Path;

   --  ------------------------------
   --  Return the SHA1 signature computed for the file.
   --  ------------------------------
   function Get_SHA1 (Element : in File_Type) return String is
   begin
      return Hex_Encoder.Transform (Element.SHA1);
   end Get_SHA1;

   --  ------------------------------
   --  Add the file with the given name in the container.
   --  ------------------------------
   overriding
   procedure Add_File (Into    : in out Default_Container;
                       Element : in File_Type) is
   begin
      Into.Files.Append (Element);
   end Add_File;

   --  ------------------------------
   --  Add the directory with the given name in the container.
   --  ------------------------------
   overriding
   procedure Add_Directory (Into    : in out Default_Container;
                            Element : in Directory_Type) is
   begin
      Into.Dirs.Append (Element);
   end Add_Directory;

   --  ------------------------------
   --  Create a new file instance with the given name in the container.
   --  ------------------------------
   overriding
   function Create (Into : in Default_Container;
                    Name : in String) return File_Type is
   begin
      return Allocate (Name => Name,
                       Dir  => Into.Current);
   end Create;

   --  ------------------------------
   --  Create a new directory instance with the given name in the container.
   --  ------------------------------
   overriding
   function Create (Into : in Default_Container;
                    Name : in String) return Directory_Type is
   begin
      return Allocate (Name => Name,
                       Dir  => Into.Current);
   end Create;

   --  ------------------------------
   --  Find the file with the given name in this file container.
   --  Returns NO_FILE if the file was not found.
   --  ------------------------------
   overriding
   function Find (From : in Default_Container;
                  Name : in String) return File_Type is
      pragma Unreferenced (From, Name);
   begin
      return NO_FILE;
   end Find;

   --  ------------------------------
   --  Find the directory with the given name in this file container.
   --  Returns NO_DIRECTORY if the directory was not found.
   --  ------------------------------
   overriding
   function Find (From : in Default_Container;
                  Name : in String) return Directory_Type is
      pragma Unreferenced (From, Name);
   begin
      return NO_DIRECTORY;
   end Find;

   --  ------------------------------
   --  Set the directory object associated with the container.
   --  ------------------------------
   procedure Set_Directory (Into      : in out Default_Container;
                            Directory : in Directory_Type) is
   begin
      Into.Current := Directory;
      Into.Files.Clear;
      Into.Dirs.Clear;
   end Set_Directory;

   --  ------------------------------
   --  Execute the Process procedure on each directory found in the container.
   --  ------------------------------
   overriding
   procedure Each_Directory (Container : in Default_Container;
                             Process   : not null access
                               procedure (Dir : in Directory_Type)) is
      Iter : Directory_Vectors.Cursor := Container.Dirs.First;
   begin
      while Directory_Vectors.Has_Element (Iter) loop
         Process (Directory_Vectors.Element (Iter));
         Directory_Vectors.Next (Iter);
      end loop;
   end Each_Directory;

   --  ------------------------------
   --  Execute the Process procedure on each file found in the container.
   --  ------------------------------
   overriding
   procedure Each_File (Container : in Default_Container;
                        Process   : not null access
                          procedure (F : in File_Type)) is
      Iter : File_Vectors.Cursor := Container.Files.First;
   begin
      while File_Vectors.Has_Element (Iter) loop
         Process (File_Vectors.Element (Iter));
         File_Vectors.Next (Iter);
      end loop;
   end Each_File;

end Babel.Files;

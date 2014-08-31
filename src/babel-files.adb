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

end Babel.Files;

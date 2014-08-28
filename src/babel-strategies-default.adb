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
with Babel.Files.Signatures;

package body Babel.Strategies.Default is

   --  Returns true if there is a directory that must be processed by the current strategy.
   overriding
   function Has_Directory (Strategy : in Default_Strategy_Type) return Boolean is
   begin
      return not Strategy.Queue.Directories.Is_Empty;
   end Has_Directory;

   --  Get the next directory that must be processed by the strategy.
   overriding
   function Peek_Directory (Strategy : in out Default_Strategy_Type) return String is
      Last : constant String := Strategy.Queue.Directories.Last_Element;
   begin
      Strategy.Queue.Directories.Delete_Last;
      return Last;
   end Peek_Directory;

   overriding
   procedure Execute (Strategy : in out Default_Strategy_Type) is
      Content : Babel.Files.Buffers.Buffer_Access := Strategy.Allocate_Buffer;
      File    : Babel.Files.File;
   begin
      Strategy.Queue.Queue.Dequeue (File, 1.0);
      Strategy.Read_File (File, Content);
      Babel.Files.Signatures.Sha1 (Content.all, File.SHA1);
      if Babel.Files.Is_Modified (File) then
         Strategy.Backup_File (File, Content);
      else
         Strategy.Release_Buffer (Content);
      end if;

   exception
      when others =>
         Strategy.Release_Buffer (Content);
         raise;
   end Execute;

   overriding
   procedure Add_File (Into    : in out Default_Strategy_Type;
                       Path    : in String;
                       Element : in Babel.Files.File) is
   begin
      Into.Queue.Add_File (Path, Element);
   end Add_File;

   overriding
   procedure Add_Directory (Into : in out Default_Strategy_Type;
                            Path : in String;
                            Name : in String) is
   begin
      Into.Queue.Add_Directory (Path, Name);
   end Add_Directory;

end Babel.Strategies.Default;

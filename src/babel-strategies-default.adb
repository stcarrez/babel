-----------------------------------------------------------------------
--  babel-strategies -- Strategies to backup files
--  Copyright (C) 2014, 2015, 2016 Stephane.Carrez
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
with Util.Encoders.SHA1;
with Util.Log.Loggers;

with Babel.Streams.Refs;
package body Babel.Strategies.Default is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Babel.Strategies.Default");

   --  Returns true if there is a directory that must be processed by the current strategy.
   overriding
   function Has_Directory (Strategy : in Default_Strategy_Type) return Boolean is
   begin
      return false; --  not Strategy.Queue.Directories.Is_Empty;
   end Has_Directory;

   --  Get the next directory that must be processed by the strategy.
   overriding
   procedure Peek_Directory (Strategy  : in out Default_Strategy_Type;
                             Directory : out Babel.Files.Directory_Type) is
      Last : constant String := ""; --  Strategy.Queue.Directories.Last_Element;
   begin
      --        Strategy.Queue.Directories.Delete_Last;
      null;
   end Peek_Directory;

   --  Set the file queue that the strategy must use.
   procedure Set_Queue (Strategy : in out Default_Strategy_Type;
                        Queue    : in Babel.Files.Queues.File_Queue_Access) is
   begin
      Strategy.Queue := Queue;
   end Set_Queue;

   overriding
   procedure Execute (Strategy : in out Default_Strategy_Type) is
      use type Babel.Files.File_Type;

      File    : Babel.Files.File_Type;
      SHA1    : Util.Encoders.SHA1.Hash_Array;
      Stream  : Babel.Streams.Refs.Stream_Ref;
   begin
      Strategy.Queue.Queue.Dequeue (File, 0.1);
      if File = Babel.Files.NO_FILE then
         Log.Debug ("Dequeue NO_FILE");
      else
         Log.Debug ("Dequeue {0}", Babel.Files.Get_Path (File));
         Strategy.Read_File (File, Stream);
         Babel.Files.Signatures.Sha1 (Stream, SHA1);
         Babel.Files.Set_Signature (File, SHA1);
         if Babel.Files.Is_Modified (File) then
            Strategy.Backup_File (File, Stream);
         end if;
      end if;

   end Execute;

   --  Scan the directory
   overriding
   procedure Scan (Strategy  : in out Default_Strategy_Type;
                   Directory : in Babel.Files.Directory_Type;
                   Container : in out Babel.Files.File_Container'Class) is

      procedure Add_Queue (File : in Babel.Files.File_Type) is
      begin
         Log.Debug ("Queueing {0}", Babel.Files.Get_Path (File));
         Strategy.Queue.Add_File (File);
      end Add_Queue;

   begin
      Strategy_Type (Strategy).Scan (Directory, Container);
      Container.Each_File (Add_Queue'Access);
   end Scan;

--     overriding
--     procedure Add_File (Into    : in out Default_Strategy_Type;
--                         Path    : in String;
--                         Element : in Babel.Files.File) is
--     begin
--        Into.Queue.Add_File (Path, Element);
--     end Add_File;
--
--     overriding
--     procedure Add_Directory (Into : in out Default_Strategy_Type;
--                              Path : in String;
--                              Name : in String) is
--     begin
--        Into.Queue.Add_Directory (Path, Name);
--     end Add_Directory;

end Babel.Strategies.Default;

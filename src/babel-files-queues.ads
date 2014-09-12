-----------------------------------------------------------------------
--  bkp-files -- File and directories
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
with Ada.Calendar;
with Ada.Directories;
with Ada.Containers.Vectors;
with Util.Encoders.SHA1;
with Util.Concurrent.Fifos;
with Util.Strings.Vectors;
with ADO;
with Babel.Base.Models;
package Babel.Files.Queues is

   --  package Babel.Strategies.Flow/Optimize/Small/Larges/Default/Immediate/Simple/Pipeline/Serial
   --  File name
   --  Directory info
   --  File ID or null
   --  File size         => sha1, file status, backup
   package File_Fifo is new Util.Concurrent.Fifos (Element_Type     => File_Type,
                                                   Default_Size     => 100,
                                                   Clear_On_Dequeue => True);
   --
   --     procedure Execute (Backup : in out Small_Files_Strategy;
   --                        Queue  : in out File_Queue) is
   --     begin
   --        loop
   --           Queue.Dequeue (File, 1.0);
   --           --  load file
   --           --  compute sha1
   --           --  if modified then backup file
   --        end loop;
   --
   --     exception
   --        when File_Fifo.Timeout =>
   --           null;
   --     end Execute;

   type File_Queue is limited new File_Container with record
      Queue       : File_Fifo.Fifo;
      Directories : Util.Strings.Vectors.Vector;
   end record;

   overriding
   procedure Add_File (Into    : in out File_Queue;
                       Path    : in String;
                       Element : in File);

   overriding
   procedure Add_Directory (Into : in out File_Queue;
                            Path : in String;
                            Name : in String);

   type Directory_Queue is limited private;

   --  Returns true if there is a directory in the queue.
   function Has_Directory (Queue : in Directory_Queue) return Boolean;

   --  Get the next directory from the queue.
   procedure Peek_Directory (Queue     : in out Directory_Queue;
                             Directory : out Directory_Type);

private

   type Directory_Queue is limited record
      Directories : Directory_Vector;
   end record;

end Babel.Files.Queues;

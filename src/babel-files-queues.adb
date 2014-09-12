-----------------------------------------------------------------------
--  babel-files-queues -- File and directory queues
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
package body Babel.Files.Queues is

   --  ------------------------------
   --  Returns true if there is a directory in the queue.
   --  ------------------------------
   function Has_Directory (Queue : in Directory_Queue) return Boolean is
   begin
      return not Queue.Directories.Is_Empty;
   end Has_Directory;

   --  ------------------------------
   --  Get the next directory from the queue.
   --  ------------------------------
   procedure Peek_Directory (Queue     : in out Directory_Queue;
                             Directory : out Directory_Type) is
   begin
      Directory := Queue.Directories.Last_Element;
      Queue.Directories.Delete_Last;
   end Peek_Directory;

end Babel.Files.Queues;

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
with Babel.Files.Queues;
package Babel.Strategies.Default is

   type Default_Strategy_Type is new Babel.Strategies.Strategy_Type with private;

   --  Returns true if there is a directory that must be processed by the current strategy.
   overriding
   function Has_Directory (Strategy : in Default_Strategy_Type) return Boolean;

   --  Get the next directory that must be processed by the strategy.
   overriding
   procedure Peek_Directory (Strategy  : in out Default_Strategy_Type;
                             Directory : out Babel.Files.Directory_Type);

   overriding
   procedure Execute (Strategy : in out Default_Strategy_Type);

   --  Scan the directory
   overriding
   procedure Scan (Strategy  : in out Default_Strategy_Type;
                   Directory : in Babel.Files.Directory_Type;
                   Container : in out Babel.Files.File_Container'Class);

   --  Set the file queue that the strategy must use.
   procedure Set_Queue (Strategy : in out Default_Strategy_Type;
                        Queue    : in Babel.Files.Queues.File_Queue_Access);

   --     overriding
--     procedure Add_File (Into    : in out Default_Strategy_Type;
--                         Path    : in String;
--                         Element : in Babel.Files.File);
--
--     overriding
--     procedure Add_Directory (Into : in out Default_Strategy_Type;
--                              Path : in String;
--                              Name : in String);

private

   type Default_Strategy_Type is new Babel.Strategies.Strategy_Type with record
      Queue : Babel.Files.Queues.File_Queue_Access;
   end record;

end Babel.Strategies.Default;

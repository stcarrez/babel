-----------------------------------------------------------------------
--  babel-commands-save -- Save command for babel
--  Copyright (C) 2014 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
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

package Babel.Commands.Save is

   --  ------------------------------
   --  Command
   --  ------------------------------
   type Command is new Babel.Commands.Command with null record;
   type Command_Access is access all Command'Class;

   --  Execute the command with the arguments.
   overriding
   procedure Execute (Cmd       : in Command);

   --  Write the help associated with the command.
   overriding
   procedure Help (Cmd       : in Command);

end Babel.Commands.Save;

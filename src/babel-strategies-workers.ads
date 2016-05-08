-----------------------------------------------------------------------
--  babel-strategies-workers -- Tasks that perform strategy work
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
generic
   type Worker_Strategy is limited new Babel.Strategies.Strategy_Type with private;
package Babel.Strategies.Workers is

   type Worker_Type (Count : Positive) is limited private;

   procedure Configure (Worker  : in out Worker_Type;
                        Process : not null access procedure (S : in out Worker_Strategy));

   procedure Start (Worker   : in out Worker_Type);

   procedure Finish (Worker   : in out Worker_Type;
                     Database : in out Babel.Base.Database'Class);

private

   task type Worker_Task is
      entry Start (Strategy : in Babel.Strategies.Strategy_Type_Access);

      entry Finish (Database : in out Babel.Base.Database'Class);
   end Worker_Task;

   type Worker_Task_Array is array (Positive range <>) of Worker_Task;
   type Strategy_Array is array (Positive range <>) of aliased Worker_Strategy;

   type Worker_Type (Count : Positive) is limited record
      Workers    : Worker_Task_Array (1 .. Count);
      Strategies : Strategy_Array (1 .. Count);
   end record;

end Babel.Strategies.Workers;

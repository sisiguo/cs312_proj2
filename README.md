## Authors: May Young, Hui Si (Sisi) Guo

# What is the problem?
We will implement the 2048 game. It is 4x4 tile grid game where the goal is to sum up tiles of powers of 2 by moving all tiles either up, down, left, or right. You win the game when you get a 2048 tile.

Browser version of 2048: http://2048game.com/

# What is the something extra?
- We will implement a visual representation of the state of the game in the console rather than it being purely text-based. At every user action, we will output a new visual representation of the state of the game to give the user a more interactive experience.

- We will implement different difficulty levels of the game: the easiest level has an unlimited number of moves. As the level increases, the number of moves is increasingly restricted.

- We will add a number of moves counter.

# What did we learn from doing this?
Functional programming is quite well suited for the task of implementing simple board games. By following the "Game Abstraction" model introduced in lecture we were able to quite smoothly implement the entire 2048 board game using the Haskell functional programming language. We were able to successfully implement all of our program requirements using Haskell.

The functional programming model of using high level functions that are proven to work for data manipulation allowed for a very concise, clean, and easy to follow implementation. This can be seen by how the main game file defining the logic behind the 2048 game is only 162 lines long, and the main game loop driver file even smaller at only 76 lines long! Implementing the equivalent functionality using an imperative language like Java would have likely at least doubled each file's line count.

In terms of scalability, we found that the "Game Abstraction" model in a functional language is very easy to scale. For example, we initially had our "State" type being only a single item - the "Board" representing the values of each board tile. It was a straight forward process to extend the "State" type to include the number of moves a player has performed so far. We simply made the "State" type that was previously just a "Board" into a tuple of "(Board, NumMoves)", with "NumMoves" being a type of "Int".

A program that is defined in a functional programming language can be easily maintained given that the number of lines of code is typically small and everything is clearly defined in the code base of a functional program. However, this is the case only if comments clearly document the purpose of each function, type, and data structure used in the program. For example, in our 2048 game implementation, we found it hard to keep track of the types that functions took in and how they worked to manipulate data if we did not clearly document the type signature and purpose for each function.
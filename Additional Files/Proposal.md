# CPSC 312 Project

# Mastermind Simulator

Our project is a simple graphical implementation of the popular board game Mastermind. Through the three different stages of this project (Product Pitch, MPV, and POC) we will try to emulate the board game’s different aspects that build on each other, with the “Product Pitch” as the apex and most complex version of the project.

## Product Pitch

Our product aims to emulate the popular code breaking board game, Mastermind. Mastermind is a popular board game, invented in 1970. The basis of the game consists of a player called the codemaker, choosing a pattern of four pegs (having the option of five different colours). The codemaker then hides their pattern or “code” from the codebreaker. The codebreaker has 12 turns to guess which pattern of pegs were chosen by the codemaker. After each turn, the codebreaker receives feedback about how many pegs had both the colour and position correct (how many red pins they receive), and how many pegs have only the correct colour (how many white pins they receive).

So if the code is (Blue, Green, Orange, Yellow) and the codebreaker guesses (Orange, Green, Purple, Yellow), the feedback they receive would be 2 red pins and one white pin.
Furthermore, we intend for our product to determine the most strategic first turn someone could have in the game of Mastermind and develop an algorithm that consistently breaks the “code” in as little turns as possible.

This project builds on our current knowledge of Haskell and what we learned, by applying the minmax technique for finding the most optimal next turn in a game as the starting point of our algorithm. We can also experiment with other techniques and algorithms when trying to find the optimal first turn one could have in a game of Mastermind or optimize the algorithms to take as little turns as it possibly can.

We will also learn a new element of Haskell by trying to implement a Graphical User Interface for the game of Mastermind. We will explore different libraries and methods of implementation when trying to build the GUI, as we want our GUI to emulate the board game as closely as possible. Learning how to work with GUIs in Haskell will help increase our individual capabilities when working with Haskell, as we won’t be limited to just using textual input outputs.

## Minimal Viable Project

Our MVP will be a single player game of Mastermind, where the user tries to guess a randomly generated “code” within 12 turns. The user will be presented with a GUI that consists of four circles(representing each peg) and four squares (representing the pins that provide feedback about the guess). The user can interact with the GUI and submit the combination of pegs once they are satisfied with their selection. After they submit their turn, the four pins will provide them with feedback. Red pins indicate correct colour and position, and white pins indicate correct colour only. The user can use the feedback to make another guess and repeat this process until they run out of turns, or they guess the code correctly.
This MVP build towards our product pitch by first providing the framework where we can build and test our algorithm on. Secondly, since this is a single player game it provided us with the foundations to change this into a 2-player game. Instead of having the “code” be randomly generated, a user would pick the “code” themselves, allowing the second user to guess what the “code” is.
This leads naturally to learning and applying some new element of the Haskell, which would be using new libraries to implement the GUI as mentioned in the “Final Products” portion. And it builds on the strength and power of Haskell, as Haskell’s unique type system allows for quick and intuitive development of game components and rapid prototyping when iterating through our game design.

## Proof of Concept

Our proof of concepts resided around the 4 pegs the user can select. The pegs will be represented by 4 red circles as the default. The user will be able to click on each of the four circles, as each time they click on a circle, it will switch to a different colour. Going from red to orange, orange to yellow, and so on. The colours available are red, orange, yellow, green, and blue.

This is an important element of the project because it is the basis of the interactive feature that the game offers. It represents a turn that the user can take during the game. Once we can implement a GUI that represents the 4 pegs and that the user interacts with, we can also represent the feedback the user receives that indicates how close they are to breaking the “code”. We can then repeat this process for the 12 turns or until the user has guessed the code correctly. That will be the bulk of our MVP, we just have to then map what each of the colours mean in our backend, so that we can build functions that generate random combination of pins (a.k.a the “code”), provide feedback about how close each guess is to the actual “code”, and determine if the guess is the correct “code”.

### How to test and run the code: Haskell

Before we get started on the command to run the code, we have some dependencies that we have to install beforehand.
Stack

- glut32.dll (Windows)
- If you are running windows: There is a file in CPSC-312-Mastermind\Additional Files called "glut32.dll". Drag and drop that file into the folder path "C:/Windows/System32."

To run the project:

- Open the folder "haskell"
- Open the terminal and enter "stack build"
- Once again in the terminal, enter "stack exec my-project-exe"

To run the tests:

- Open the folder "haskell"
- Open the terminal and enter "stack build --test"

##
## EPITECH PROJECT, 2021
## B-PDG-300-MPL-3-1-PDGD04am-hugo.gardes
## File description:
## Makefile
##

C++	=	g++

NAME	=	enNesprit

SRC		=	$(wildcard *.cpp)
SRC		+=	$(wildcard src/*.cpp)

RM	=	rm -f

CPPFLAGS	=	-lX11 -lGL -lpthread -lpng -lstdc++fs -std=c++17 -g3 -I ./headers -I ./lib/

all:	$(NAME)

$(NAME):
	$(C++) -o $(NAME) $(SRC) $(CPPFLAGS)

clean:
	$(RM) *~
	$(RM) #*#

fclean:	clean
	$(RM) $(NAME)

re:	fclean all

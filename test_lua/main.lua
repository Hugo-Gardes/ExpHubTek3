local mots = {"PROGRAMMATION", "ORDINATEUR", "LUA", "TERMINAL"}


-- local mots = {}
-- local mots_file = io.open("mots.txt", "r")
-- for ligne in mots_file:lines() do
--    table.insert(mots, ligne)
--end
--mots_file:close()


local function choisir_mot()
    math.randomseed(os.time())
    return mots[math.random(#mots)]
end

local function afficher_lettres(lettres_trouvees, mot)
    local affichage = ""
    for i = 1, #mot do
        local lettre = mot:sub(i, i)
        if lettres_trouvees[lettre] then
            affichage = affichage .. lettre .. " "
        else
            affichage = affichage .. "_ "
        end
    end
    return affichage
end

local function jouer_pendu()
    local mot = choisir_mot()
    local lettres_trouvees = {}
    local lettres_ratees = {}
    local nb_erreurs = 0
    while nb_erreurs < 6 do
        print(afficher_lettres(lettres_trouvees, mot))
        local message = "Lettres déjà utilisées : "
        for lettre, _ in pairs(lettres_trouvees) do
            message = message .. lettre .. " "
        end
        for lettre, _ in pairs(lettres_ratees) do
            message = message .. lettre .. " "
        end
        print(message)
        io.write("Proposez une lettre : ")
        local lettre = io.read():upper():sub(1, 1)
        if lettres_trouvees[lettre] or lettres_ratees[lettre] then
            print("Vous avez déjà proposé cette lettre.")
        elseif mot:find(lettre, 1, true) then
            print("Bien joué ! La lettre " .. lettre .. " est dans le mot.")
            lettres_trouvees[lettre] = true
            if afficher_lettres(lettres_trouvees, mot):gsub(" ", "") == mot then
                print("Félicitations ! Vous avez deviné le mot " .. mot .. " !")
                return
            end
        else
            print("Dommage, la lettre " .. lettre .. " n'est pas dans le mot.")
            lettres_ratees[lettre] = true
            nb_erreurs = nb_erreurs + 1
        end
        print("---------")
        if nb_erreurs > 0 then
            print("|       |")
            print("|       O")
            if nb_erreurs > 1 then
                if nb_erreurs == 2 then
                    print("|       |")
                elseif nb_erreurs == 3 then
                    print("|      /|")
                else
                    print("|      /|\\")
                end
                if nb_erreurs > 4 then
                    print("|      /")
                else
                    print("|")
                end
            else
                print("|")
            end
        else
            print("|")
            print("|")
            print("|")
        end
        print("|")
        print("=========")
    end
    print("Dommage, vous avez perdu. Le mot était : " .. mot)
end

jouer_pendu()
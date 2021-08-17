## Example 1
This example will fetch the data of [Pikachu](https://app.pokemon-api.xyz/pokemon/pikachu) from [pokemon API](https://purukitto.github.io/pokemon-api/) and then parse the JSON data using the library and a template JSON.

The raw JSON:
``` JSON
{
    "id": 25,
    "name": {
        "english": "Pikachu",
        "japanese": "ピカチュウ",
        "chinese": "皮卡丘",
        "french": "Pikachu"
    },
    "type": [
        "Electric"
    ],
    "base": {
        "HP": 35,
        "Attack": 55,
        "Defense": 40,
        "Sp. Attack": 50,
        "Sp. Defense": 50,
        "Speed": 90
    },
    "species": "Mouse Pokémon",
    "description": "While sleeping, it generates electricity in the sacs in its cheeks. If it’s not getting enough sleep, it will be able to use only weak electricity.",
    "evolution": {
        "prev": [
            "172",
            "high Friendship"
        ],
        "next": [
            [
                "26",
                "use Thunder Stone"
            ]
        ]
    },
    "profile": {
        "height": "0.4 m",
        "weight": "6 kg",
        "egg": [
            "Field",
            "Fairy"
        ],
        "ability": [
            [
                "Static",
                "false"
            ],
            [
                "Lightning Rod",
                "true"
            ]
        ],
        "gender": "50:50"
    },
    "image": {},
    "sprite": "https://raw.githubusercontent.com/Purukitto/pokemon-data.json/master/images/pokedex/sprites/025.png",
    "thumbnail": "https://raw.githubusercontent.com/Purukitto/pokemon-data.json/master/images/pokedex/thumbnails/025.png",
    "hires": "https://raw.githubusercontent.com/Purukitto/pokemon-data.json/master/images/pokedex/hires/025.png"
}
```

We want to remodel the JSON according to the following template:
``` TemplateJSON
{
    "id": {{$.id}},
    "name": {{$.name.english}},
    "description": {{$.description}},
    "profile": {
        "height": {{$.profile.height}},
        "weight": {{$.profile.weight}},
        "hp": {{$.base.HP}},
        "attack": {{$.base.Attack}},
        "defence": {{$.base.Defense}},
        "spAttack": {{$.base.Sp. Attack}},
        "spDefence": {{$.base.Sp. Defense}},
        "speed": {{$.base.Speed}}
    }
}
```

The final parsed JSON looks like the following:
``` JSON
{
    "id": 25,
    "name": "Pikachu",
    "description": "While sleeping, it generates electricity in the sacs in its cheeks. If it’s not getting enough sleep, it will be able to use only weak electricity.",
    "profile": {
        "height": "0.4 m",
        "weight": "6 kg",
        "hp": 35,
        "attack": 55,
        "defence": 40,
        "spAttack": "",
        "spDefence": "",
        "speed": 90
    }
}
```
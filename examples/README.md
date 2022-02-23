## Example 1
This example will fetch the data of few random users from `source.json` and then parse the JSON data using the library and a template JSON.

We want to remodel the JSON according to the following template:
``` TemplateJSON
{{ range i, x := $.results }}
  {
    "id": {{i}},
    "fullName": {{concatName x.name}},
    "profile": {
      "gender": {{getG x.gender}},
      "emailID": {{x.email}},
      "isSuperUser": {{isAdmin x.login.username}}
    }
  }
{{ end }}
```
We are using some custom functions in the template:
- `concatName`: this function concats the first name, last name and title and produces a string.
- `getG`: this return the character representation of the gender.
- `isAdmin`: this function checks if the user is a super user and returna a boolean.

These functions are supplied as a hashmap to `runKritiWith`.

The final parsed JSON will look something like the following:
``` JSON
[
        {
        "id": 1,
        "fullName": "Dr. Sheldon Cooper",
        "profile": {
            "gender": "M",
            "emailID": "s.cooperphd@yahoo.com ",
            "isAdmin": true
        }
    }
]
```
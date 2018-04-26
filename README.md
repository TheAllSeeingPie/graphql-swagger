# graphql-swagger
#### Or probably more accurately: Swagger to GraphQL!

This project aims to convert a Swagger definition to a GraphQL schema.

This is very very very early days in the project and it's more of a prototype at the moment. Testing is quite rough and ready and definitely needs to be expanded to match a lot more edge cases.

Currently it's very naive and can only parse a sub-set of the Swagger specification. It will require a fair amount of work to be able to cope with more complex schemas.

It relies on the fact that the definitions are held in a reference, and this will need to change for more simplistic schemas.

There are quite a few assumptions being made at the moment and these hopefully will be come more generalised in the future.

Feel free to run the example and play around with the `HelloWorld.swagger` and `HelloWorld.json` files in an attempt to get started with this project.

This project is focused on the use-case of hosted `.yaml` files and not the, often more populer, `.json` format.
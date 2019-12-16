# PPS-18-justcards ![](https://github.com/PPS-1819-Group/PPS-18-justcards/workflows/JustCards%20CI/badge.svg)
JustCards is an online card gaming platform. It will make available some default games, allowing users to create their own.

In order to start the application via Gradle you have to digit on a CLI the following instructions:

**Client application**
`gradle runClient -q --console=plain`

With optional param -Pserver="serverAddress" if the server address is known

**Server application**
`gradle runServer -q --console=plain`

With optional param -Paddress="serverAddress" if the address is known
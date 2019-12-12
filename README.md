# PPS-18-justcards ![](https://github.com/PPS-1819-Group/PPS-18-justcards/workflows/JustCards%20CI/badge.svg)
JustCards is an online card gaming platform. It will make available some default games, allowing users to create their own.

In order to start the application via Gradle you have to digit on a CLI the following instructions:

**Client application**
`gradle runClient -q --console=plain`

The client must know server IP address or URL and must set it on `org.justcards.client.ClientApp`

**Server application**
`gradle runServer -q --console=plain`

The server must know it's IP address or URL and must be set at the voice `canonical.hostname` on server.conf in the resource files
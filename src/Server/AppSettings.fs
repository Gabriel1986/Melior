module Server.AppSettings

[<CLIMutable>]
type DatabaseSettings = {
    ConnectionString: string
}

[<CLIMutable>]
type AppSettings = {
    Database: DatabaseSettings
}
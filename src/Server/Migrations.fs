module rec Server.Migrations

open System.Reflection
open Npgsql
open SimpleMigrations
open SimpleMigrations.DatabaseProvider

let  run (logger: Serilog.ILogger) (connectionString: string) : unit =
    using (new NpgsqlConnection(connectionString)) (fun connection ->
        try
            let migrationsAssembly = Assembly.GetAssembly(typeof<CreateInitialTables>)
            let databaseProvider = PostgresqlDatabaseProvider (connection)
            let migrator = SimpleMigrator (migrationsAssembly, databaseProvider)

            logger.Information "Migrating DB"

            migrator.Load()
            migrator.MigrateToLatest()

            logger.Information  "Finished migrating db"

        with exn ->
            let exceptionType = exn.GetType().Name
            let exceptionMessage = exn.Message

            logger.Error(exn, sprintf "An '%s' exception with message '%s' happened during migration" exceptionType exceptionMessage)
    )

[<Migration(1L, "Create initial tables")>]
type CreateInitialTables() =
    inherit Migration()
    override u.Up () =
        u.Execute(
            """
                CREATE TABLE Persons (
                    PersonId UUID PRIMARY KEY,
                    FirstName VARCHAR(255),
                    LastName VARCHAR(255),
                    LanguageCode VARCHAR(16),
                    Gender VARCHAR(16),
                    Title VARCHAR(32),
                    MainAddress JSON NOT NULL,
                    ContactAddress JSONB,
                    OtherAddresses JSONB,
                    MainTelephoneNumber VARCHAR(32),
                    MainTelephoneNumberComment VARCHAR(255),
                    MainEmailAddress VARCHAR(255),
                    MainEmailAddressComment VARCHAR(255),
                    OtherContactMethods JSONB
                );
                CREATE TABLE ProfessionalSyndics (
                    SyndicId UUID PRIMARY KEY,
                    PersonId UUID References Persons(PersonId),
                    IsActive BOOLEAN DEFAULT TRUE
                );

                CREATE TABLE Buildings (
                    BuildingId UUID PRIMARY KEY,
                    IsActive BOOLEAN DEFAULT TRUE,
                    Code VARCHAR(16) NOT NULL,
                    Name VARCHAR(255) NOT NULL,
                    Address JSONB NOT NULL,
                    OrganizationNumber VARCHAR(12),
                    Remarks VARCHAR,
                    GeneralMeetingFrom DATE,
                    GeneralMeetingUntil DATE,
                    ConciergePersonId UUID REFERENCES Persons (PersonId),
                    ConciergeOwnerId UUID REFERENCES Persons (PersonId),
                    SyndicOwnerId UUID REFERENCES Persons (PersonId),
                    SyndicProfessionalSyndicId UUID REFERENCES ProfessionalSyndics (SyndicId),
                    SyndicPersonId UUID REFERENCES Persons (PersonId),
                    YearOfConstruction INT,
                    YearOfDelivery INT
                );

                CREATE TABLE Owners (
                    PersonId UUID References Persons(PersonId),
                    BuildingId UUID References Buildings(BuildingId),
                    IsActive BOOLEAN DEFAULT TRUE,
                    IsResident BOOLEAN DEFAULT TRUE,
                    PRIMARY KEY (PersonId, BuildingId)
                );
                CREATE INDEX idx_Owners_PersonId ON Owners(PersonId);
                CREATE INDEX idx_Owners_BuildingId ON Owners(BuildingId);

                CREATE TABLE OrganizationTypes (
                    OrganizationTypeId UUID PRIMARY KEY,
                    Name VARCHAR(255)
                );

                CREATE TABLE Organizations (
                    OrganizationId UUID PRIMARY KEY,
                    OrganizationNumber VARCHAR(12),
                    IsActive Boolean Default TRUE,
                    OrganizationTypeId UUID References OrganizationTypes(OrganizationTypeId),
                    Name VARCHAR(255),
                    Address JSONB NOT NULL,
                    MainContactPersonId UUID References Persons(PersonId)
                );
                CREATE TABLE OrganizationContactPersons (
                    OrganizationId UUID References Organizations(OrganizationId),
                    PersonId UUID References Persons(PersonId),
                    RoleWithinOrganization VARCHAR(32),
                    PRIMARY KEY (OrganizationId, PersonId)
                );
                CREATE INDEX idx_OrganizationContactPersons_OrganizationId ON OrganizationContactPersons(OrganizationId);

                CREATE TABLE Lots (
                    LotId UUID PRIMARY KEY,
                    BuildingId UUID References Buildings(BuildingId),
                    CurrentOwnerPersonId UUID References Persons(PersonId),
                    CurrentOwnerOrganizationId UUID References Organizations(OrganizationId),
                    Code VARCHAR(16) NOT NULL,
                    LotType VARCHAR(32) NOT NULL,
                    Description VARCHAR,
                    Floor INT,
                    Surface INT,
                    IsActive Boolean DEFAULT TRUE
                );
                CREATE INDEX idx_Lots_CurrentOwnerPersonId ON Lots(CurrentOwnerPersonId);
                CREATE INDEX idx_Lots_CurrentOwnerOrganizationId ON Lots(CurrentOwnerOrganizationId);
                CREATE INDEX idx_Lots_BuildingId ON Owners(BuildingId);
            """
        )
    override u.Down () = u.Execute("DROP TABLE logs")
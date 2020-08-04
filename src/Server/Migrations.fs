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

                CREATE TABLE Organizations (
                    OrganizationId UUID PRIMARY KEY,
                    BuildingId UUID,
                    OrganizationNumber VARCHAR(12),
                    VatNumber VARCHAR(64),
                    VatNumberVerifiedOn DATE,
                    IsActive Boolean Default TRUE,
                    Name VARCHAR(255) NOT NULL,
                    Address JSONB NOT NULL,
                    MainTelephoneNumber VARCHAR(32),
                    MainTelephoneNumberComment VARCHAR(255),
                    MainEmailAddress VARCHAR(255),
                    MainEmailAddressComment VARCHAR(255),
                    OtherContactMethods JSONB
                );

                CREATE TABLE ProfessionalSyndics (
                    OrganizationId UUID PRIMARY KEY References Organizations(OrganizationId),
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
                    ConciergeOwnerId UUID,
                    ConciergePersonId UUID REFERENCES Persons(PersonId),
                    SyndicOwnerId UUID,
                    SyndicProfessionalSyndicId UUID REFERENCES ProfessionalSyndics(OrganizationId),
                    SyndicPersonId UUID REFERENCES Persons(PersonId),
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

                CREATE TABLE Lots (
                    LotId UUID PRIMARY KEY,
                    BuildingId UUID References Buildings(BuildingId),
                    Code VARCHAR(16) NOT NULL,
                    LotType VARCHAR(32) NOT NULL,
                    Description VARCHAR,
                    Floor INT,
                    Surface INT,
                    IsActive Boolean DEFAULT TRUE
                );
                CREATE INDEX idx_Lots_BuildingId ON Owners(BuildingId);

                CREATE TABLE LotOwners (
                    LotId UUID PRIMARY KEY,
                    OrganizationId UUID References Organizations(OrganizationId), 
                    PersonId UUID References Persons(PersonId),
                    Role VARCHAR(32)
                );

                CREATE INDEX idx_Lots_CurrentOwnerPersonId ON Lots(CurrentOwnerPersonId);
                CREATE INDEX idx_Lots_CurrentOwnerOrganizationId ON Lots(CurrentOwnerOrganizationId);


                CREATE TABLE OrganizationTypes (
                    OrganizationTypeId UUID PRIMARY KEY,
                    Name VARCHAR(255) NOT NULL
                );

                CREATE TABLE OrganizationOrganizationTypeLinks (
                    OrganizationId UUID References Organizations(OrganizationId),
                    OrganizationTypeId UUID References OrganizationTypes(OrganizationTypeId),
                    PRIMARY KEY (OrganizationId, OrganizationTypeId)
                );
                CREATE INDEX idx_OrganizationOrganizationTypeLinks_OrganizationId ON OrganizationOrganizationTypeLinks(OrganizationId);

                CREATE TABLE OrganizationContactPersons (
                    OrganizationId UUID References Organizations(OrganizationId),
                    PersonId UUID References Persons(PersonId),
                    RoleWithinOrganization VARCHAR(32),
                    IsActive BOOLEAN DEFAULT TRUE,
                    PRIMARY KEY (OrganizationId, PersonId)
                );

                ALTER TABLE Organizations ADD CONSTRAINT FK_Organizations_Buildings
                    FOREIGN KEY (BuildingId)
                    REFERENCES Buildings(BuildingId);

                ALTER TABLE Buildings ADD CONSTRAINT fk_ConciergeOwnerId 
                    FOREIGN KEY (BuildingId, ConciergeOwnerId) 
                    REFERENCES Owners(BuildingId, PersonId);

                ALTER TABLE Buildings ADD CONSTRAINT fk_SyndicOwnerId
                    FOREIGN KEY (BuildingId, SyndicOwnerId)
                    REFERENCES Owners(BuildingId, PersonId);

                CREATE INDEX idx_OrganizationContactPersons_OrganizationId ON OrganizationContactPersons(OrganizationId);
            """
        )
    override u.Down () = failwith "Not supported"

[<Migration(2L, "Create mediafile tables")>]
type CreateMediaFileTables() =
    inherit Migration()
    override u.Up () =
        u.Execute(
            """
                CREATE TABLE MediaFiles (
                    Partition VARCHAR(64) NOT NULL,
                    EntityId UUID NOT NULL,
                    FileId UUID PRIMARY KEY,
                    FileName VARCHAR(255),
                    FileSize INT,
                    MimeType VARCHAR(64),
                    UploadedOn TIMESTAMP
                );
                CREATE INDEX idx_MediaFiles_Partition_EntityId ON MediaFiles(Partition, EntityId);
            """
        )
    override u.Down () = failwith "Not supported"

[<Migration(3L, "Create user tables")>]
type CreateUserTables() =
    inherit Migration()
    override u.Up () =
        u.Execute(
            """
                CREATE TABLE Users (
                    UserId UUID PRIMARY KEY,
                    DisplayName VARCHAR(255),
                    EmailAddress VARCHAR(255) UNIQUE,
                    PreferredLanguageCode VARCHAR(12),
                    PasswordHash BYTEA,
                    UseTwoFac Boolean,
                    TwoFacSecret BYTEA,
                    IsActive Boolean DEFAULT TRUE
                );
                CREATE INDEX idx_Users_EmailAddress ON Users(EmailAddress);

                CREATE TABLE RecoveryCodes (
                    EmailAddress VARCHAR(255),
                    RecoveryCodeHash BYTEA,
                    PRIMARY KEY (EmailAddress, RecoveryCodeHash)
                );
                CREATE INDEX idx_RecoveryCodes_EmailAddress ON RecoveryCodes(EmailAddress);

                CREATE TABLE FailedTwoFacEvents (
                    UserId UUID,
                    TimeStamp TIMESTAMP
                );
                CREATE INDEX idx_FailedTwoFacEvents_UserId ON FailedTwoFacEvents(UserId);
            """
        )
    override u.Down () = failwith "Not supported"

CREATE TABLE [dbo].[Words] (
    [Id]    INT           NOT NULL IDENTITY(0, 1),
    [Type]  NVARCHAR (50) NOT NULL,
    [Word]  NVARCHAR (50) NOT NULL,
    [Value] INT           NOT NULL,
    PRIMARY KEY CLUSTERED ([Id] ASC)
);


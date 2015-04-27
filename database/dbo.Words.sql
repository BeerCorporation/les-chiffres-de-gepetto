CREATE TABLE [dbo].[Words] (
    [Id]    INT           NOT NULL,
    [Type]  NVARCHAR (50) NOT NULL,
    [Word]  NVARCHAR (50) NOT NULL,
    [Value] INT           NOT NULL,
    PRIMARY KEY CLUSTERED ([Id] ASC)
);


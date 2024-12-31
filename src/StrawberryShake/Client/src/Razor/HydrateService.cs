using System.Text.Json;
using Microsoft.AspNetCore.Components;

namespace StrawberryShake.Razor;

public sealed class HydrateService(PersistentComponentState persistentState, IStoreAccessor storeAccessor)
    : IHydrateService
    , IDisposable
{
    private static readonly JsonSerializerOptions options = new()
    {
        PropertyNamingPolicy = JsonNamingPolicy.CamelCase
    };

    private readonly Dictionary<string, List<OperationDto>> _operations = [];
    private PersistingComponentStateSubscription? _persistingSubscription;

    public void Dispose()
    {
        _persistingSubscription?.Dispose();
    }

    public void Persist(OperationRequest request, IOperationResult result)
    {
        if (result.DataInfo is null || result.DataType.FullName is null)
        {
            return;
        }

        var operation = new OperationDto
        (
            Variables: request.Variables.ToDictionary(),
            DataInfo: JsonSerializer.Serialize<object>(result.DataInfo, options),
            ResultType: $"{result.DataType.FullName}, {result.DataType.Assembly.GetName().Name}"
        );

        if (_operations.TryGetValue(request.Name, out var operations))
        {
            operations.Add(operation);
        }
        else
        {
            _operations.Add(request.Name, [operation]);
        }

        _persistingSubscription ??= persistentState.RegisterOnPersisting(PersistOperationsAsync);
    }

    public void Restore(string operationName, Func<JsonElement, IOperationResultDataInfo> buildData)
    {
        if (!persistentState.TryTakeFromJson<List<OperationDto>>($"{nameof(HydrateService)}_{operationName}", out var operations))
        {
            return;
        }

        if (operations is null)
        {
            return;
        }

        foreach (var operation in operations)
        {
            var resultType = Type.GetType(operation.ResultType);
            var dataInfoElement = JsonSerializer.Deserialize<JsonElement?>(operation.DataInfo, options);

            if (resultType is null || dataInfoElement is null)
            {
                continue;
            }

            var dataInfo = buildData((JsonElement)dataInfoElement);

            var resultDataFactory = storeAccessor
                .GetOperationResultDataFactory(resultType);

            var request = storeAccessor
                .GetOperationRequestFactory(resultType)
                .Create(operation.Variables);

            var result = OperationResult.Create(
                resultDataFactory.Create(dataInfo),
                resultType,
                dataInfo,
                resultDataFactory,
                null);

            storeAccessor.OperationStore.Set(request, result);
        }
    }

    private Task PersistOperationsAsync()
    {
        foreach (var (key, operations) in _operations)
        {
            persistentState.PersistAsJson($"{nameof(HydrateService)}_{key}", operations);
        };

        return Task.CompletedTask;
    }

    private sealed record OperationDto(Dictionary<string, object?> Variables, string ResultType, string DataInfo);
}
using System.Text.Json;

namespace StrawberryShake.Razor;

public interface IHydrateService
{
    void Persist(OperationRequest request, IOperationResult result);
    void Restore(string operationName, Func<JsonElement, IOperationResultDataInfo> buildData);
}

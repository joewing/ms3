
#include <iostream>
#include <jsoncpp/json/json.h>

#include "Simulator.hh"

/** Parse the input.
 * Input JSON format:
 *  {
 *      "bram_count": bram_count,
 *      "kernels": [
 *          { "data": [ ... ] }, ...
 *      ],
 *      "queues": [
 *          { "id": id,
 *            "word_size": word_size }, ...
 *      ]
 *  }
 */
static bool ParseInput(Simulator *sim)
{
    Json::Reader reader;
    Json::Value root;
    if(!reader.parse(std::cin, root)) {
        std::cerr << "error: could not parse JSON\n";
        return false;
    }

    const uint32_t bram_count = root["bram_count"].asUInt();
    sim->SetBRAMCount(bram_count);

    const Json::Value kernels = root["kernels"];
    const Json::ArrayIndex kernel_count = kernels.size();
    for(Json::ArrayIndex i = 0; i < kernel_count; i++) {
        const Json::Value kernel = kernels[i];
        const Json::Value data = kernel["data"];
        const Json::ArrayIndex dcount = data.size();
        std::vector<uint32_t> values(dcount);
        for(Json::ArrayIndex j = 0; j < dcount; j++) {
            values[j] = data[j].asUInt();
        }
        sim->AddKernel(values);
    }
    const Json::Value queues = root["queues"];
    const Json::ArrayIndex queue_count = queues.size();
    for(Json::ArrayIndex i = 0; i < queue_count; i++) {
        const Json::Value queue = queues[i];
        const uint32_t id = queue["id"].asUInt();
        const uint32_t word_size = queue["word_size"].asUInt();
        sim->AddQueue(id, word_size);
    }

    return true;
}

/** Output results.
 * Output JSON format:
 *  {
 *      "total": total,
 *      "queues": [
 *          { "id": id, "depth": depth }, ...
 *      ]
 *  }
 */
static void OutputResults(const Simulator &sim, const uint32_t t)
{
    std::cout << "{\"total\": " << t << ",\n";
    std::cout << "\"queues\": [";
    std::vector<std::pair<uint32_t, uint32_t> > depths = sim.GetDepths();
    for(size_t i = 0; i < depths.size(); i++) {
        const std::pair<uint32_t, uint32_t> p = depths[i];
        std::cout << "{\"id\": " << p.first << ", ";
        std::cout << "\"depth\": " << p.second << "}";
        if(i + 1 < depths.size()) {
            std::cout << ", ";
        }
    }
    std::cout << "]}\n";
}

int main(int argc, char *argv[])
{

    // Parse input.
    Simulator sim;
    std::cout << "Reading input ... \n";
    if(!ParseInput(&sim)) {
        return -1;
    }

    // Perform the simulation.
    std::cout << "Running ... \n";
    const uint64_t t = sim.Run();

    // Output the results.
    std::cout << "Outputting results ... \n";
    OutputResults(sim, t);

    return 0;
}


#include <iostream>
#include <jsoncpp/json/json.h>
#include <cassert>

#include "Simulator.hh"

/** Parse the input.
 * Input JSON format:
 *  {
 *      "bram_count": bram_count,
 *      "kernels": [
 *          { "type": type, ... }, ...
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

    uint32_t last_count = 0;
    const Json::Value kernels = root["kernels"];
    const Json::ArrayIndex kernel_count = kernels.size();
    for(Json::ArrayIndex i = 0; i < kernel_count; i++) {
        const Json::Value kernel = kernels[i];
        const std::string t = kernel["type"].asString();
        if(t == "trace") {
            const bool last = kernel["last"].asBool();
            const Json::Value data = kernel["data"];
            const Json::ArrayIndex dcount = data.size();
            std::vector<uint32_t> values(dcount);
            for(Json::ArrayIndex j = 0; j < dcount; j++) {
                values[j] = data[j].asUInt();
            }
            if(last) {
                last_count += 1;
            }
            sim->AddTrace(values, last);
        } else if(t == "split") {
            const uint32_t in = kernel["in"].asUInt();
            const uint32_t out0 = kernel["out0"].asUInt();
            const uint32_t out1 = kernel["out1"].asUInt();
            sim->AddSplit(in, out0, out1);
        } else {
            std::cerr << "error: invalid kernel type: " << t << "\n";
            return false;
        }
    }
    assert(last_count == 1);
    const Json::Value queues = root["queues"];
    const Json::ArrayIndex queue_count = queues.size();
    for(Json::ArrayIndex i = 0; i < queue_count; i++) {
        const Json::Value queue = queues[i];
        const uint32_t id = queue["id"].asUInt();
        const uint32_t word_size = queue["word_size"].asUInt();
        assert(id > 0);
        assert(word_size > 0);
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
static void OutputResults(const Simulator &sim, const uint64_t t)
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
    if(!ParseInput(&sim)) {
        return -1;
    }

    // Perform the simulation.
    const uint64_t t = sim.Run();

    // Output the results.
    OutputResults(sim, t);

    return 0;
}

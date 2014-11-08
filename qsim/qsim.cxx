
#include <iostream>
#include <jsoncpp/json/json.h>

#include "Simulator.hh"

int main(int argc, char *argv[])
{

    std::string str;
    std::getline(std::cin, str, '\0');

    Json::Reader reader;
    Json::Value root;
    if(!reader.parse(str, root)) {
        std::cerr << "error: could not parse JSON\n";
        return -1;
    }

    Simulator sim;

    // Parse the input.
    /* Input JSON format:
        {
            "bram_count": bram_count,
            "kernels": [
                { "data": [ ... ] }, ...
            ],
            "queues": [
                { "id": id,
                  "word_size": word_size }, ...
            ]
        }
     */
    const uint32_t bram_count = root["bram_count"].asUInt();
    const Json::Value kernels = root["kernels"];
    const Json::ArrayIndex kernel_count = kernels.size();
    for(Json::ArrayIndex i = 0; i < kernel_count; i++) {
        const Json::Value kernel = kernels[i];
        const Json::Value data = kernel["data"];
        const Json::ArrayIndex dcount = data.size();
        std::vector<uint32_t> values;
        for(Json::ArrayIndex j = 0; j < dcount; j++) {
            values.push_back(data[j].asUInt());
        }
        sim.AddKernel(values);
    }
    const Json::Value queues = root["queues"];
    const Json::ArrayIndex queue_count = queues.size();
    for(Json::ArrayIndex i = 0; i < queue_count; i++) {
        const Json::Value queue = queues[i];
        const uint32_t id = queue["id"].asUInt();
        const uint32_t word_size = queue["word_size"].asUInt();
        sim.AddQueue(id, word_size);
    }

    // Perform the simulation.
    const uint64_t t = sim.Run(bram_count);

    // Output the results.
    /* Output JSON format:
        {
            "total": total,
            "queues": [
                { "id": id, "depth": depth }, ...
            ]
        }
     */
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

    return 0;
}

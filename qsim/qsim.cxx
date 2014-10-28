
#include <iostream>
#include <jsoncpp/json/json.h>

#include "Simulator.h"

int main(int argc, char *argv[])
{

    std::string str;
    std::getline(std::cin, str, '\0');

    /* Input JSON format:
        {
            "bram_count": bram_count,
            "queues": [
                { "count": count, "word_size": word_size,
                  "ptime": ptime, "pvar": pvar,
                  "ctime": ctime, "cvar": cvar }, ...
            ]
        }
     */
    Json::Reader reader;
    Json::Value root;
    if(!reader.parse(str, root)) {
        std::cerr << "error: could not parse JSON\n";
        return -1;
    }

    Simulator sim(5);
    const uint32_t bram_count = root["bram_count"].asUInt();
    const Json::Value queues = root["queues"];
    const Json::ArrayIndex size = queues.size();
    for(Json::ArrayIndex i = 0; i < size; i++) {
        const Json::Value queue = queues[i];
        const uint32_t count = queue["count"].asUInt();
        const uint32_t word_size = queue["word_size"].asUInt();
        const double ptime = queue["ptime"].asDouble();
        const double pvar = queue["pvar"].asDouble();
        const double ctime = queue["ctime"].asDouble();
        const double cvar = queue["cvar"].asDouble();
        sim.AddQueue(count, word_size, ptime, pvar, ctime, cvar);
    }

    /* Output JSON format:
        {
            "total": total,
            "depths": [ depth, ... ]
        }
     */
    const uint64_t t = sim.Run(bram_count);
    std::cout << "{\"total\": " << t << ",\n";
    std::cout << "\"depths\": [";
    std::vector<uint32_t> depths = sim.GetDepths();
    for(size_t i = 0; i < depths.size(); i++) {
        std::cout << depths[i];
        if(i + 1 < depths.size()) {
            std::cout << ", ";
        }
    }
    std::cout << "]}\n";

    return 0;
}

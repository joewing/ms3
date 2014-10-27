
#include <iostream>
#include <jsoncpp/json/json.h>

#include "Random.h"
#include "Simulator.h"

int main(int argc, char *argv[])
{

    std::string str;
    std::getline(std::cin, str, '\0');

    /* JSON format:
        [
            { 'count': count,
              'ptime': ptime, 'pvar': pvar,
              'ctime': ctime, 'cvar': cvar }, ...
        ]
     */

    Random r(5);
    for(int i = 0; i < 10; i++) {
        std::cout << r.normal_double() << std::endl;
        //std::cout << r.uniform_double(0.0, 1.0) << std::endl;
    }

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

    const uint64_t t = sim.Run(bram_count);
    std::cout << "total: " << t << "\n";
    std::vector<uint32_t> depths = sim.GetDepths();
    for(size_t i = 0; i < depths.size(); i++) {
        std::cout << "depth" << i << ": " << depths[i] << "\n";
    }

    return 0;
}

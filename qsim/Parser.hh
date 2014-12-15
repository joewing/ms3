
#ifndef PARSER_HH_
#define PARSER_HH_

#include "Simulator.hh"
#include <stdio.h>
#include <string>
#include <sstream>

class Parser
{
private:

    struct ParseError
    {
        ParseError(const std::string &m) : message(m)
        {
        }

        std::string message;
    };

public:

    Parser(Simulator *sim) : m_sim(sim)
    {
    }

    ~Parser()
    {
    }

    bool Run()
    {
        try {
            Advance();
            ParseTop();
            return true;
        } catch(const ParseError &pe) {
            fprintf(stderr, "error: %s\n", pe.message.c_str());
            return false;
        }
    }

private:

    void ParseTop()
    {
        Match('{');
        for(;;) {
            const std::string key = ParseString();
            Match(':');
            if(key == "bram_count") {
                ParseBramCount();
            } else if(key == "kernels") {
                ParseKernelList();
            } else if(key == "queues") {
                ParseQueueList();
            } else {
                throw ParseError("unexpected key at top-level");
            }
            if(m_current != ',') {
                break;
            }
            Advance();
        }
        Match('}');
    }

    void ParseBramCount()
    {
        const uint32_t bram_count = ParseUInt();
        m_sim->SetBRAMCount(bram_count);
    }

    void ParseKernelList()
    {
        Match('[');
        for(;;) {
            ParseKernel();
            if(m_current != ',') {
                break;
            }
            Advance();
        }
        Match(']');
    }

    void ParseQueueList()
    {
        Match('[');
        for(;;) {
            ParseQueue();
            if(m_current != ',') {
                break;
            }
            Advance();
        }
        Match(']');
    }

    void ParseKernel()
    {
        std::vector<uint32_t> values;
        bool is_split = false;
        bool is_last = false;
        uint32_t in = 0;
        uint32_t out0 = 0;
        uint32_t out1 = 0;
        Match('{');
        for(;;) {
            const std::string key = ParseString();
            Match(':');
            if(key == "type") {
                const std::string type = ParseString();
                if(type == "split") {
                    is_split = true;
                }
            } else if(key == "score") {
                ParseUInt();
            } else if(key == "last") {
                is_last = ParseBool();
            } else if(key == "id") {
                ParseUInt();
            } else if(key == "data") {
                ParseValues(&values);
            } else if(key == "in") {
                in = ParseUInt();
            } else if(key == "out0") {
                out0 = ParseUInt();
            } else if(key == "out1") {
                out1 = ParseUInt();
            } else {
                throw ParseError("invalid kernel key");
            }
            if(m_current != ',') {
                break;
            }
            Advance();
        }
        Match('}');
        if(is_split) {
            m_sim->AddSplit(in, out0, out1);
        } else {
            m_sim->AddTrace(values, is_last);
        }
    }

    void ParseValues(std::vector<uint32_t> *values)
    {
        Match('[');
        for(;;) {
            values->push_back(ParseUInt());
            if(m_current != ',') {
                break;
            }
            Advance();
        }
        Match(']');
    }

    void ParseQueue()
    {
        uint32_t id = 0;
        uint32_t word_size = 4;
        Match('{');
        for(;;) {
            const std::string key = ParseString();
            Match(':');
            if(key == "word_size") {
                word_size = ParseUInt();
            } else if(key == "id") {
                id = ParseUInt();
            } else {
                throw ParseError("invalid queue key");
            }
            if(m_current != ',') {
                break;
            }
            Advance();
        }
        Match('}');
        assert(id > 0);
        assert(word_size > 0);
        m_sim->AddQueue(id, word_size);
    }

    uint32_t ParseUInt()
    {
        uint32_t result = 0;
        while(m_current >= '0' && m_current <= '9') {
            result *= 10;
            result += uint32_t(m_current - '0');
            Advance();
        }
        return result;
    }

    bool ParseBool()
    {
        if(m_current == 'f') {
            Match('f');
            Match('a');
            Match('l');
            Match('s');
            Match('e');
            return false;
        } else if(m_current == 't') {
            Match('t');
            Match('r');
            Match('u');
            Match('e');
            return true;
        } else {
            throw ParseError("expected true or false");
        }
    }

    std::string ParseString()
    {
        std::stringstream ss;
        Match('\"');
        while(m_current != '\"') {
            ss << m_current;
            Advance();
        }
        Match('\"');
        return ss.str();
    }

    void Match(const char ch)
    {
        if(m_current != ch) {
            std::stringstream ss;
            ss << "got '" << m_current << "' but expected '" << ch << "'";
            throw ParseError(ss.str());
        }
        Advance();
    }

    bool IsSpace(const char ch)
    {
        switch(ch) {
        case ' ':
        case '\n':
        case '\t':
        case '\r':
            return true;
        default:
            return false;
        }
    }

    void Advance()
    {
        do {
            m_current = fgetc(stdin);
        } while(IsSpace(m_current));
    }

    Simulator * const m_sim;
    char m_current;

};

#endif

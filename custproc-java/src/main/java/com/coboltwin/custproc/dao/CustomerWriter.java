package com.coboltwin.custproc.dao;

import com.coboltwin.custproc.model.CustomerRecord;

public interface CustomerWriter extends AutoCloseable {
    void write(CustomerRecord record);
}

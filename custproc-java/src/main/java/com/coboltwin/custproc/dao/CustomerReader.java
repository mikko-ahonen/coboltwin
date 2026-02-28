package com.coboltwin.custproc.dao;

import com.coboltwin.custproc.model.CustomerRecord;

public interface CustomerReader extends Iterable<CustomerRecord>, AutoCloseable {
}

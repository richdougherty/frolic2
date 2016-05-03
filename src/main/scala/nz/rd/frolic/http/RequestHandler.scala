package nz.rd.frolic.http

import nz.rd.frolic.async.AFunc

final class RequestHandler(val f: AFunc[Request, Response])